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
;; DNel is an Emacs package that implements a Desktop Notifications
;; server in pure Lisp, aspiring to be a small, but flexible drop-in
;; replacement for standalone daemons like Dunst. Active notifications
;; are tracked in the global variable `dnel-state' whenever the global
;; minor mode `dnel-mode' is active. Users are free to monitor the
;; contents of that variable as they see fit, though DNel does provide
;; some additional convenience functions.

;;; Code:
(require 'dbus)

(defconst dnel--path "/org/freedesktop/Notifications")
(defconst dnel--service (subst-char-in-string ?/ ?. (substring dnel--path 1)))
(defconst dnel--interface dnel--service)

;;;###autoload
(define-minor-mode dnel-mode
  "Act as a Desktop Notifications server and track notifications."
  :global t :lighter " DNel"
  (funcall (if dnel-mode #'dnel--start-server #'dnel--stop-server) dnel-state))

(defvar dnel-state (list 0)
  "The minor mode tracks all active desktop notifications here.

This cons cell's car is the count of distinct IDs assigned so far,
its cdr is a list of currently active notifications, newest first.

Each notification, in turn, is a cons cell: its car is the ID,
its cdr is a property list of the notification's attributes.")

(defun dnel-invoke-action (state id &optional action)
  "Invoke ACTION of the notification identified by ID in STATE.

ACTION defaults to the key \"default\"."
  (let ((notification (dnel-get-notification id state)))
    (when notification
      (dnel--dbus-talk-to (plist-get (cdr notification) 'client)
                          'send-signal 'ActionInvoked id
                          (or action "default")))))

(defun dnel-close-notification (state id &optional reason)
  "Close the notification identified by ID in STATE for REASON.

REASON defaults to 3 (i.e., closed by call to CloseNotification)."
  (let* ((notification (dnel-get-notification id state t))
         (reason (or reason 3)))
    (if (not notification) (when (= reason 3) (signal 'dbus-error ()))
      (run-hooks 'dnel-state-changed-hook)
      (dnel--dbus-talk-to (plist-get (cdr notification) 'client)
                          'send-signal 'NotificationClosed id reason)))
  :ignore)

(defun dnel-format-notification (notification state)
  "Propertize notification NOTIFICATION in STATE."
  (let ((get (apply-partially #'plist-get (cdr notification))))
    (format "%s [%s: %s]"
            (propertize (number-to-string (car notification)) 'invisible t)
            (funcall get 'app-name)
            (apply #'dnel--format-summary (funcall get 'summary)
                   (car notification) state (mapcar get '(body actions))))))

(defun dnel--format-summary (summary id state &optional body actions)
  "Propertize SUMMARY for notification identified by ID in STATE.

The optional BODY is shown as a tooltip, ACTIONS can be selected from a menu."
  (let ((controls `((mouse-1 . ,(lambda () (interactive)
                                  (dnel-invoke-action state id)))
                    (down-mouse-2 . ,(dnel--format-actions actions id state))
                    (mouse-3 . ,(lambda () (interactive)
                                  (dnel-close-notification state id 2))))))
    (apply #'propertize summary 'mouse-face 'mode-line-highlight
           'local-map `(keymap (header-line keymap . ,controls)
                               (mode-line keymap . ,controls) . ,controls)
           (when (and body (not (string-equal "" body))) `(help-echo ,body)))))

(defun dnel--format-actions (actions id state)
  "Propertize ACTIONS for notification identified by ID in STATE."
  (let ((result (list 'keymap)))
    (dotimes (i (/ (length actions) 2))
      (let ((key (pop actions)))
        (push (list i 'menu-item (pop actions)
                    (lambda () (interactive)
                      (dnel-invoke-action state id key)))
              result)))
    (reverse (cons "Actions" result))))

(defun dnel--start-server (state)
  "Register server for keeping track of notifications in STATE."
  (dolist (args `((Notify ,(apply-partially #'dnel--notify state) t)
                  (CloseNotification
                   ,(apply-partially #'dnel-close-notification state) t)
                  (GetServerInformation
                   ,(lambda () (list "DNel" "sinic" "0.1" "1.2")) t)
                  (GetCapabilities ,(lambda () '(("body" "actions"))) t)))
    (apply #'dnel--dbus-talk 'register-method args))
  (dbus-register-service :session dnel--service))

(defun dnel--stop-server (state)
  "Close all notifications in STATE, then unregister server."
  (while (cdr state)
    (dnel-close-notification state (caadr state) 2))  ; pops (cdr state)
  (dbus-unregister-service :session dnel--service))

(defun dnel--notify (state app-name replaces-id app-icon summary body actions
                           hints expire-timeout)
  "Handle call by introducing notification to STATE, return ID.

APP-NAME, REPLACES-ID, APP-ICON, SUMMARY, BODY, ACTIONS, HINTS, EXPIRE-TIMEOUT
are the received values as described in the Desktop Notification standard."
  (let* ((id (or (unless (zerop replaces-id)
                   (car (dnel-get-notification replaces-id state t)))
                 (setcar state (1+ (car state)))))
         (client (dbus-event-service-name last-input-event))
         (timer (when (> expire-timeout 0)
                  (run-at-time (/ expire-timeout 1000.0) nil
                               #'dnel-close-notification state id 1))))
    (push (list id 'app-name app-name 'summary summary 'body body 'client client
                'timer timer 'actions actions 'app-icon app-icon 'hints hints)
          (cdr state))
    (run-hooks 'dnel-state-changed-hook)
    id))

;; Timers call this function, so keep an eye on complexity:
(defun dnel-get-notification (id state &optional remove)
  "Return notification identified by ID in STATE.

The returned notification is deleted from STATE if REMOVE is non-nil."
  (while (and (cdr state) (/= id (caadr state)))
    (setq state (cdr state)))
  (if (not remove) (cadr state)
    (let ((timer (plist-get (cdadr state) 'timer)))
      (when timer (cancel-timer timer)))
    (pop (cdr state))))

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
