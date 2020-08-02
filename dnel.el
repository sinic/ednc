;;; dnel.el --- Emacs Desktop Notifications server -*- lexical-binding: t; -*-
;; Copyright (C) 2020 Simon Nicolussi

;; Author: Simon Nicolussi <sinic@sinic.name>
;; Version: 0.1
;; Package-Requires: ((emacs "24.4"))
;; Keywords: unix
;; Homepage: https://github.com/sinic/dnel

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; This package implements a Desktop Notifications server in Emacs Lisp,
;; aiming to be a drop-in replacement for standalone daemons like Dunst.
;; Active notifications are tracked in the variable `dnel-notifications'
;; whenever the global minor mode `dnel-mode' is active.  A convenience
;; function `dnel-get-default-propertized-string' is provided, suitable
;; for inclusion in the user's mode-line, for example like this:
;;    (nconc global-mode-string '((:eval (dnel-get-default-propertized-string
;;                                        dnel-notifications))))
;;    (add-hook 'dnel-notifications-changed-hook
;;              (lambda () (force-mode-line-update t)))

;;; Code:
(require 'dbus)

(defconst dnel--path "/org/freedesktop/Notifications")
(defconst dnel--service (subst-char-in-string ?/ ?. (substring dnel--path 1)))
(defconst dnel--interface dnel--service)

;;;###autoload
(define-minor-mode dnel-mode
  "Act as a Desktop Notifications server and track notifications."
  :global t :lighter " DNel"
  (funcall (if dnel-mode #'dnel--start-server #'dnel--stop-server)
           dnel-notifications))

(defvar dnel-notifications (list 0)
  "The minor mode tracks all active desktop notifications here.

This cons cell's car is the count of distinct IDs assigned so far,
its cdr is a list of currently active notifications, newest first.

Each notification, in turn, is a cons cell: its car is the ID,
its cdr is a property list of the notification's attributes.")

;; Not called anywhere, but suggested for the user's mode-line:
(defun dnel-get-default-propertized-string (active &optional hide)
  "Return a propertized string describing notifications in ACTIVE.

Only notifications from applications not listed in HIDE are considered.

The exact format of the returned string is subject to change."
  (mapconcat (lambda (notification)
               (let ((app-name (plist-get (cdr notification) 'app-name)))
                 (if (member app-name hide) ""
                   (push app-name hide)
                   (dnel--format-notification notification active))))
             (cdr active) ""))

(defun dnel-invoke-action (id active &optional action)
  "Invoke ACTION of the notification identified by ID in ACTIVE.

ACTION defaults to the key \"default\"."
  (let ((notification (dnel--get-notification id active)))
    (dnel--dbus-talk-to (plist-get (cdr notification) 'client)
                        'send-signal 'ActionInvoked id (or action "default"))))

(defun dnel-close-notification (active id &optional reason)
  "Close the notification identified by ID in ACTIVE for REASON.

REASON defaults to 3 (i.e., closed by call to CloseNotification)."
  (let ((notification (dnel--get-notification id active t)))
    (run-hooks 'dnel-notifications-changed-hook)
    (dnel--dbus-talk-to (plist-get (cdr notification) 'client)
                        'send-signal 'NotificationClosed id (or reason 3))))

(defun dnel--format-notification (notification active)
  "Propertize notification NOTIFICATION in ACTIVE."
  (let ((get (apply-partially #'plist-get (cdr notification))))
    (format " [%s: %s]" (funcall get 'app-name)
            (apply #'dnel--format-summary (funcall get 'summary)
                   (car notification) active (mapcar get '(body actions))))))

(defun dnel--format-summary (summary id active &optional body actions)
  "Propertize SUMMARY for notification identified by ID in ACTIVE.

The optional BODY is shown as a tooltip, ACTIONS can be selected from a menu."
  (let ((controls `((mouse-1 . ,(lambda () (interactive)
                                  (dnel-invoke-action id active)))
                    (down-mouse-2 . ,(dnel--format-actions actions id active))
                    (mouse-3 . ,(lambda () (interactive)
                                  (dnel-close-notification active id 2))))))
    (apply #'propertize summary 'mouse-face 'mode-line-highlight
           'local-map `(keymap (header-line keymap . ,controls)
                               (mode-line keymap . ,controls))
           (when (and body (not (string-empty-p body))) `(help-echo ,body)))))

(defun dnel--format-actions (actions id active)
  "Propertize ACTIONS for notification identified by ID in ACTIVE."
  (let ((result (list 'keymap)))
    (dotimes (i (/ (length actions) 2))
      (let ((key (pop actions)))
        (push (list i 'menu-item (pop actions)
                    (lambda () (interactive)
                      (dnel-invoke-action id active key)))
              result)))
    (reverse (cons "Actions" result))))

(defun dnel--start-server (active)
  "Register server for keeping track of notifications in ACTIVE."
  (dolist (args `((Notify ,(apply-partially #'dnel--notify active) t)
                  (CloseNotification
                   ,(apply-partially #'dnel-close-notification active) t)
                  (GetServerInformation
                   ,(lambda () (list "Emacs" "GNU" emacs-version "1.2")) t)
                  (GetCapabilities ,(lambda () '(("body" "actions"))) t)))
    (apply #'dnel--dbus-talk 'register-method args))
  (dbus-register-service :session dnel--service))

(defun dnel--stop-server (active)
  "Close all notifications in ACTIVE, then unregister server."
  (while (cdr active)
    (dnel-close-notification active (caadr active) 2))  ; pops (cdr active)
  (dbus-unregister-service :session dnel--service))

(defun dnel--notify (active app-name replaces-id _app-icon summary body actions
                            _hints expire-timeout)
  "Handle call by introducing notification to ACTIVE, return ID.

APP-NAME, REPLACES-ID, _APP-ICON, SUMMARY, BODY, ACTIONS, _HINTS, EXPIRE-TIMEOUT
are the received values as described in the Desktop Notification standard.

_APP-ICON and _HINTS are ignored for now."
  (let* ((id (if (zerop replaces-id) (setcar active (1+ (car active)))
               (car (dnel--get-notification replaces-id active t))))
         (client (dbus-event-service-name last-input-event))
         (timer (when (> expire-timeout 0)
                  (run-at-time (/ expire-timeout 1000.0) nil
                               #'dnel-close-notification active id 1))))
    (push (list id 'client client 'timer timer 'app-name app-name 'summary
                summary 'body body 'actions actions)
          (cdr active))
    (run-hooks 'dnel-notifications-changed-hook)
    id))

;; Timers call this function, so keep an eye on complexity:
(defun dnel--get-notification (id active &optional remove)
  "Return notification identified by ID in ACTIVE.

The returned notification is deleted from ACTIVE if REMOVE is non-nil."
  (while (and (cdr active) (/= id (caadr active)))
    (setq active (cdr active)))
  (if (not (cdr active)) (signal 'dbus-error ())  ; not found
    (if (not remove) (cadr active)
      (let ((timer (plist-get (cdadr active) 'timer)))
        (when timer (cancel-timer timer)))
      (pop (cdr active)))))

(defun dnel--dbus-talk-to (service suffix symbol &rest rest)
  "Help with most actions involving D-Bus service SERVICE.

If SERVICE is nil, then a service name is derived from `last-input-event'.

SUFFIX is the non-constant suffix of a D-Bus function (e.g., `call-method'),
SYMBOL is named after the function's first argument (e.g., `GetServerInfo'),
REST contains the remaining arguments to that function."
  (let ((full (intern (concat "dbus-" (symbol-name suffix)))))
    (apply full :session (or service (dbus-event-service-name last-input-event))
           dnel--path dnel--interface (symbol-name symbol) rest)))

(defun dnel--dbus-talk (suffix symbol &rest rest)
  "Help with most actions involving D-Bus service `dnel--service'.

SUFFIX is the non-constant suffix of a D-Bus function (e.g., `call-method'),
SYMBOL is named after the function's first argument (e.g., `GetServerInfo'),
REST contains the remaining arguments to that function."
  (apply #'dnel--dbus-talk-to dnel--service suffix symbol rest))

(provide 'dnel)
;;; dnel.el ends here
