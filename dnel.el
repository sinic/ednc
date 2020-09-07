;;; dnel.el --- Emacs Desktop Notifications server -*- lexical-binding: t; -*-
;; Copyright (C) 2020 Simon Nicolussi

;; Author: Simon Nicolussi <sinic@sinic.name>
;; Version: 0.1
;; Package-Requires: ((emacs "26.1"))
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
;; replacement for standalone daemons like Dunst.  Active notifications
;; are tracked whenever the global minor mode `dnel-mode' is active and
;; can be retrieved as a list with the function `dnel-notifications'.
;; DNel also provides a hook `dnel-notifications-changed-functions', so
;; that users can handle newly added and removed notifications as they
;; see fit.  By default, past and present notifications are recorded in
;; the interactive log buffer `*dnel-log*'.

;;; Code:
(require 'cl-lib)
(require 'dbus)

(defconst dnel--log-name "*dnel-log*")

(defconst dnel--path "/org/freedesktop/Notifications")
(defconst dnel--service (subst-char-in-string ?/ ?. (substring dnel--path 1)))
(defconst dnel--interface dnel--service)

(cl-defstruct (dnel-notification (:constructor dnel--notification-create)
                                 (:copier nil))
  id app-name summary body actions image hints timer client
  log-position pop-suffix)

;;;###autoload
(define-minor-mode dnel-mode
  "Act as a Desktop Notifications server and track notifications."
  :global t :lighter " DNel"
  (if dnel-mode (dnel--start-server) (dnel--stop-server)))

(defvar dnel-notifications-changed-functions #'dnel--update-log-buffer
  "Functions in this list are called on changes to notifications.

Their arguments are the changed notification, and an optional flag.
The notification was added if the flag is nil, or removed otherwise.")

(defvar dnel--state (list 0)
  "The minor mode tracks all active desktop notifications here.

This object is currently implemented as a cons cell: its car is the
count of distinct IDs assigned so far, its cdr is a list of currently
active notifications, newest first.")

(defun dnel-notifications ()
  "Return currently active notifications."
  (cdr dnel--state))

(defun dnel-invoke-action (notification &optional action)
  "Invoke ACTION of the NOTIFICATION.

ACTION defaults to the key \"default\"."
  (dnel--dbus-talk-to (dnel-notification-client notification) 'send-signal
                      'ActionInvoked (dnel-notification-id notification)
                      (or action "default")))

(defun dnel-close-notification (notification &optional reason)
  "Close the NOTIFICATION for REASON.

REASON defaults to 2 (i.e., dismissed by user)."
  (dnel--delete-notification notification)
  (run-hook-with-args 'dnel-notifications-changed-functions notification t)
  (dnel--dbus-talk-to (dnel-notification-client notification) 'send-signal
                      'NotificationClosed (dnel-notification-id notification)
                      (or reason 2)))

(defun dnel--close-notification-by-id (id)
  "Close the notification identified by ID."
  (let ((found (cl-find id (cdr dnel--state) :test #'eq
                        :key #'dnel-notification-id)))
    (if found (dnel-close-notification found 3)
      (signal 'dbus-error ()))) :ignore)

(defun dnel-format-notification (notification &optional full)
  "Return propertized description of NOTIFICATION.

If FULL is non-nil, include details, possibly across multiple lines."
  (let* ((hints (dnel-notification-hints notification))
         (urgency (or (dnel--get-hint hints "urgency") 1))
         (inherit (if (<= urgency 0) 'shadow (if (>= urgency 2) 'bold))))
    (format (propertize " %s[%s: %s]%s" 'face (list :inherit inherit))
            (propertize " " 'display (dnel-notification-image notification))
            (dnel-notification-app-name notification)
            (dnel--format-summary notification full)
            (if full (concat "\n" (dnel-notification-body notification) "\n")
              ""))))

(defun dnel--format-summary (notification &optional full)
  "Return propertized summary of NOTIFICATION.

If FULL is nil, link to the log, otherwise include a menu for actions."
  (let ((summary (dnel-notification-summary notification))
        (controls
         `((mouse-1 . ,(lambda () (interactive)
                         (dnel-invoke-action notification)))
           ,(if full `(down-mouse-2 . ,(dnel--get-actions-keymap notification))
              `(mouse-2 . ,(lambda () (interactive)
                             (dnel--pop-to-log-buffer notification))))
           (mouse-3 . ,(lambda () (interactive)
                         (dnel-close-notification notification))))))
    (propertize summary 'mouse-face 'mode-line-highlight 'local-map
                `(keymap (header-line keymap . ,controls)
                         (mode-line keymap . ,controls) . ,controls))))

(defun dnel--get-actions-keymap (notification)
  "Return keymap for actions of NOTIFICATION."
  (let ((actions (dnel-notification-actions notification))
        (result (list 'keymap)))
    (dotimes (i (/ (length actions) 2))
      (let ((key (pop actions)))
        (push (list i 'menu-item (pop actions)
                    (lambda () (interactive)
                      (dnel-invoke-action notification key)))
              result)))
    (reverse (cons "Actions" result))))

(defun dnel--start-server ()
  "Register server to keep track of notifications in `dnel--state'."
  (dolist (args `((Notify ,#'dnel--notify t)
                  (CloseNotification ,#'dnel--close-notification-by-id t)
                  (GetServerInformation
                   ,(lambda () (list "DNel" "sinic" "0.1" "1.2")) t)
                  (GetCapabilities ,(lambda () '(("body" "actions"))) t)))
    (apply #'dnel--dbus-talk 'register-method args))
  (dbus-register-service :session dnel--service))

(defun dnel--stop-server ()
  "Close all notifications, then unregister server."
  (mapc #'dnel-close-notification (cdr dnel--state))
  (dbus-unregister-service :session dnel--service))

(defun dnel--notify (app-name replaces-id app-icon summary body actions
                              hints expire-timeout)
  "Handle call by introducing a new notification and return its ID.

APP-NAME, REPLACES-ID, APP-ICON, SUMMARY, BODY, ACTIONS, HINTS, EXPIRE-TIMEOUT
are the received values as described in the Desktop Notification standard."
  (let ((new (dnel--notification-create
              :app-name app-name :summary summary :body body :actions actions
              :image (dnel--get-image hints app-icon) :hints hints
              :client (dbus-event-service-name last-input-event))))
    (setf (dnel-notification-id new)
          (or (unless (zerop replaces-id)
                (let ((found (cl-find replaces-id (cdr dnel--state) :test #'eq
                                      :key #'dnel-notification-id)))
                  (when found (dnel--delete-notification found) replaces-id)))
              (cl-incf (car dnel--state))))
    (if (> expire-timeout 0)
        (setf (dnel-notification-timer new)
              (run-at-time (/ expire-timeout 1000.0) nil
                           #'dnel-close-notification new 1)))
    (dnel--push-notification new)
    (run-hook-with-args 'dnel-notifications-changed-functions new)
    (dnel-notification-id new)))

(defun dnel--get-image (hints app-icon)
  "Return image descriptor created from HINTS or from APP-ICON.

This function is destructive."
  (let ((image (or (dnel--data-to-image (dnel--get-hint hints "image-data" t))
                   (dnel--path-to-image (dnel--get-hint hints "image-path" t))
                   (dnel--path-to-image app-icon)
                   (dnel--data-to-image (dnel--get-hint hints "icon_data" t)))))
    (if image (setf (image-property image :max-height) (line-pixel-height)
                    (image-property image :ascent) 90))
    image))

(defun dnel--get-hint (hints key &optional remove)
  "Return and delete from HINTS the value specified by KEY.

The returned value is removed from HINTS if REMOVE is non-nil."
  (let* ((pair (assoc key hints))
         (tail (cdr pair)))
    (if (and remove pair) (setcdr pair nil))
    (caar tail)))

(defun dnel--path-to-image (image-path)
  "Return image descriptor created from file URI IMAGE-PATH."
  (let ((prefix "file://"))
    (if (and (stringp image-path) (> (length image-path) (length prefix))
             (string-equal (substring image-path 0 (length prefix)) prefix))
        (create-image (substring image-path (length prefix))))))

(defun dnel--data-to-image (image-data)
  "Return image descriptor created from raw (iiibiiay) IMAGE-DATA.

This function is destructive."
  (if image-data
      (cl-destructuring-bind (width height row-stride _ bit-depth channels data)
          image-data
        (when (and (= bit-depth 8) (<= 3 channels 4))
          (dnel--delete-padding data (* channels width) row-stride)
          (dnel--delete-padding data 3 channels)
          (let ((header (format "P6\n%d %d\n255\n" width height)))
            (create-image (apply #'unibyte-string (append header data))
                          'pbm t))))))

(defun dnel--delete-padding (list payload total)
  "Delete LIST elements between multiples of PAYLOAD and TOTAL.

This function is destructive."
  (if (< payload total)
      (let ((cell (cons nil list))
            (delete (if (and (= payload 3) (= total 4)) #'cddr  ; fast opcode
                      (apply-partially #'nthcdr (- total payload -1))))
            (keep (if (= payload 3) #'cdddr (apply-partially #'nthcdr payload))))
        (while (cdr cell)
          (setcdr (setq cell (funcall keep cell)) (funcall delete cell))))))

(defun dnel--push-notification (notification)
  "Push NOTIFICATION to parent state `dnel--state'."
  (let ((state dnel--state))
    (setf (dnel-notification-pop-suffix notification) state)
    (let ((next (cadr state)))
      (push notification (cdr state))
      (if next (setf (dnel-notification-pop-suffix next) (cdr state))))))

(defun dnel--delete-notification (notification)
  "Delete NOTIFICATION from parent state and return it."
  (let ((suffix (dnel-notification-pop-suffix notification)))
    (let ((timer (dnel-notification-timer notification)))
      (if timer (cancel-timer timer)))
    (let ((next (caddr suffix)))
      (if next (setf (dnel-notification-pop-suffix next) suffix)))
    (pop (cdr suffix))))

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

(defmacro dnel--with-log-buffer (&optional buffer &rest body)
  "Execute BODY with log BUFFER or with a newly initialized one."
  (declare (indent 1))
  `(with-current-buffer (or ,buffer (generate-new-buffer dnel--log-name))
     (let ((inhibit-read-only t))
       (unless ,buffer
         (special-mode)
         (save-excursion
           (dolist (notification (reverse (cdr dnel--state)))
             (setf (dnel-notification-log-position notification) (point))
             (insert (dnel-format-notification notification t) ?\n))))
       ,@body)))

(defun dnel--update-log-buffer (notification &optional removep)
  "Update log buffer to reflect new status of NOTIFICATION.

If REMOVEP is nil, NOTIFICATION was added, otherwise it was removed."
  (let ((buffer (get-buffer dnel--log-name)))
    (dnel--with-log-buffer buffer
      (if buffer (save-excursion
                   (dnel--update-log notification removep))))))

(defun dnel--pop-to-log-buffer (&optional notification)
  "Pop to log buffer and (optionally) move point to NOTIFICATION."
  (let ((buffer (get-buffer dnel--log-name))
        (position (if notification
                      (dnel-notification-log-position notification))))
    (dnel--with-log-buffer buffer
      (pop-to-buffer (current-buffer))
      (if position (goto-char position)))))

(defun dnel--update-log (notification &optional removep)
  "Update current buffer to reflect new status of NOTIFICATION.

If REMOVEP is nil, NOTIFICATION was added, otherwise it was removed."
  (let ((old (dnel-notification-log-position notification)))
    (if old (add-text-properties (goto-char old) (line-end-position)
                                 '(face (:strike-through t) local-map ()))))
  (unless removep
    (setf (dnel-notification-log-position notification) (goto-char (point-max)))
    (insert (dnel-format-notification notification t) ?\n)))

(provide 'dnel)
;;; dnel.el ends here
