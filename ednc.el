;;; ednc.el --- Emacs Desktop Notifications server -*- lexical-binding: t; -*-
;; Copyright (C) 2020 Simon Nicolussi

;; Author: Simon Nicolussi <sinic@sinic.name>
;; Version: 0.1
;; Package-Requires: ((emacs "26.1"))
;; Keywords: unix
;; Homepage: https://github.com/sinic/ednc

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
;; EDNC is an Emacs package that implements a Desktop Notifications
;; server in pure Lisp, aspiring to be a small, but flexible drop-in
;; replacement for standalone daemons like Dunst.  Active notifications
;; are tracked whenever the global minor mode `ednc-mode' is active and
;; can be retrieved as a list with the function `ednc-notifications'.
;; EDNC also provides a hook `ednc-notifications-changed-functions', so
;; that users can handle newly added and removed notifications as they
;; see fit.  To be useful out of the box, EDNC records past and present
;; notifications in the interactive log buffer `*ednc-log*'.

;;; Code:
(require 'cl-lib)
(require 'dbus)

(defconst ednc-log-name "*ednc-log*")

(defconst ednc--path "/org/freedesktop/Notifications")
(defconst ednc--service (subst-char-in-string ?/ ?. (substring ednc--path 1)))
(defconst ednc--interface ednc--service)

(cl-defstruct (ednc-notification (:constructor ednc--notification-create)
                                 (:copier nil))
  id app-name summary body actions image hints timer client controls
  ednc-tracked ednc-logged)

;;;###autoload
(define-minor-mode ednc-mode
  "Act as a Desktop Notifications server and track notifications."
  :global t :lighter " EDNC"
  (if ednc-mode (ednc--start-server) (ednc--stop-server)))

(defvar ednc-notifications-changed-functions #'ednc--update-log-buffer
  "Functions in this list are called on changes to notifications.

Their arguments are the removed notification, if any,
followed by the newly added notification, if any.")

(defvar ednc--state (list 0)
  "The minor mode tracks all active desktop notifications here.

This object is currently implemented as a cons cell: its car is the
count of distinct IDs assigned so far, its cdr is a list of currently
active notifications, newest first.")

(defvar ednc-log-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "RET") #'ednc-invoke-action)
    (define-key map (kbd "TAB") #'ednc-toggle-body-visibility)
    (define-key map "d" #'ednc-dismiss-notification)
    map)
  "Keymap for the EDNC log buffer.")

(defun ednc-notifications ()
  "Return currently active notifications."
  (cdr ednc--state))

(defun ednc-invoke-action (notification &optional action)
  "Invoke ACTION of the NOTIFICATION.

ACTION defaults to the key \"default\"."
  (interactive (list (get-text-property (point) 'ednc-notification)))
  (unless (and notification (ednc-notification-ednc-tracked notification))
    (user-error "No active notification at point"))
  (ednc--dbus-talk-to (ednc-notification-client notification) 'dbus-send-signal
                      "ActionInvoked" (ednc-notification-id notification)
                      (or action "default")))

(defun ednc-dismiss-notification (notification)
  "Dismiss the NOTIFICATION."
  (interactive (list (get-text-property (point) 'ednc-notification)))
  (unless (and notification (ednc-notification-ednc-tracked notification))
    (user-error "No active notification at point"))
  (ednc--close-notification notification 2))

(defun ednc-toggle-body-visibility (position)
  "Toggle visibility of the body of notification at POSITION."
  (interactive "d")
  (let ((prop 'ednc-notification))
    (unless (or (get-text-property position prop)
                (if (> position 1) (get-text-property (cl-decf position) prop)))
      (user-error "No notification at or before position"))
    (let* ((end (or (next-single-property-change position prop) (point-max)))
           (begin (or (previous-single-property-change end prop) (point-min)))
           (eol (save-excursion (goto-char begin) (line-end-position)))
           (current (get-text-property eol 'invisible))
           (inhibit-read-only t))
      (if (< eol end) (put-text-property eol end 'invisible (not current))))))

(defun ednc--close-notification-by-id (id)
  "Close the notification identified by ID."
  (let ((found (cl-find id (cdr ednc--state)
                        :test #'eq :key #'ednc-notification-id)))
    (if found (ednc--close-notification found 3) (signal 'dbus-error nil)))
  :ignore)

(defun ednc--close-notification (notification reason)
  "Close the NOTIFICATION for REASON."
  (ednc--delete-notification notification)
  (run-hook-with-args 'ednc-notifications-changed-functions notification nil)
  (ednc--dbus-talk-to (ednc-notification-client notification) 'dbus-send-signal
                      "NotificationClosed" (ednc-notification-id notification)
                      reason))

(defun ednc-format-notification (notification)
  "Return propertized description of NOTIFICATION."
  (let* ((hints (ednc-notification-hints notification))
         (urgency (or (ednc--get-hint hints "urgency") 1))
         (inherit (if (<= urgency 0) 'shadow (if (>= urgency 2) 'bold))))
    (format (propertize " %s[%s: %s]%s" 'face (list :inherit inherit)
                        'ednc-notification notification)
            (propertize " " 'display (ednc-notification-image notification))
            (ednc-notification-app-name notification)
            (ednc--format-summary notification)
            (propertize (concat "\n" (ednc-notification-body notification) "\n")
                        'invisible t))))

(defun ednc--format-summary (notification)
  "Return propertized summary of NOTIFICATION."
  (let ((summary (ednc-notification-summary notification))
        (controls (ednc-notification-controls notification)))
    (propertize summary 'mouse-face 'mode-line-highlight 'keymap
                `(keymap (header-line keymap . ,controls)
                         (mode-line keymap . ,controls) . ,controls))))

(defun ednc--set-default-controls (notification)
  "Add default mouse controls to NOTIFICATION."
  (setf (ednc-notification-controls notification)
        `((mouse-1 . ,(lambda () (interactive)
                        (ednc-invoke-action notification)))
          (down-mouse-2 . ,(ednc--get-actions-keymap notification))
          (mouse-3 . ,(lambda () (interactive)
                        (ednc-dismiss-notification notification))))))

(defun ednc--add-log-controls (notification)
  "Add mouse controls for log navigation to NOTIFICATION."
  (push `(C-mouse-1 . ,(lambda () (interactive)
                         (ednc-pop-to-notification-in-log-buffer notification)))
        (ednc-notification-controls notification)))

(defun ednc--get-actions-keymap (notification)
  "Return keymap for actions of NOTIFICATION."
  (cl-loop with in = (ednc-notification-actions notification) and out for i by 1
           while in do (push (let ((key (pop in)))
                               (list i 'menu-item (pop in)
                                     (lambda () (interactive)
                                       (ednc-invoke-action notification key))))
                             out)
           finally return (cons 'keymap (nreverse (cons "Actions" out)))))

(defun ednc--start-server ()
  "Register server to keep track of notifications in `ednc--state'."
  (dolist (args `(("Notify" ,#'ednc--notify t)
                  ("CloseNotification" ,#'ednc--close-notification-by-id t)
                  ("GetServerInformation"
                   ,(lambda () (list "EDNC" "sinic" "0.1" "1.2")) t)
                  ("GetCapabilities" ,(lambda () '(("body" "actions"))) t)))
    (apply #'ednc--dbus-talk 'dbus-register-method args))
  (dbus-register-service :session ednc--service))

(defun ednc--stop-server ()
  "Dismiss all notifications, then unregister server."
  (mapc #'ednc-dismiss-notification (cdr ednc--state))
  (dbus-unregister-service :session ednc--service))

(defun ednc--notify (app-name replaces-id app-icon summary body actions
                              hints expire-timeout)
  "Handle call by introducing a new notification and return its ID.

APP-NAME, REPLACES-ID, APP-ICON, SUMMARY, BODY, ACTIONS, HINTS, EXPIRE-TIMEOUT
are the received values as described in the Desktop Notification standard."
  (let* ((old (if (> replaces-id 0)
                  (cl-find replaces-id (cdr ednc--state)
                           :test #'eq :key #'ednc-notification-id)))
         (new (ednc--notification-create
               :id (if old replaces-id (cl-incf (car ednc--state)))
               :app-name app-name :summary summary :body body :actions actions
               :image (ednc--get-image hints app-icon) :hints hints
               :client (dbus-event-service-name last-input-event))))
    (if (> expire-timeout 0)
        (setf (ednc-notification-timer new)
              (run-at-time (/ expire-timeout 1000.0) nil
                           #'ednc--close-notification new 1)))
    (ednc--set-default-controls new)
    (if old (ednc--delete-notification old))
    (ednc--push-notification new)
    (run-hook-with-args 'ednc-notifications-changed-functions old new)
    (ednc-notification-id new)))

(defun ednc--get-image (hints app-icon)
  "Return image descriptor created from HINTS or from APP-ICON.

This function is destructive."
  (let ((image (or (ednc--data-to-image (ednc--get-hint hints "image-data" t))
                   (ednc--path-to-image (ednc--get-hint hints "image-path" t))
                   (ednc--path-to-image app-icon)
                   (ednc--data-to-image (ednc--get-hint hints "icon_data" t)))))
    (if image (setf (image-property image :max-height) (line-pixel-height)
                    (image-property image :ascent) 90))
    image))

(defun ednc--get-hint (hints key &optional remove)
  "Return and delete from HINTS the value specified by KEY.

The returned value is removed from HINTS if REMOVE is non-nil."
  (let* ((pair (assoc key hints))
         (tail (cdr pair)))
    (if (and remove pair) (setcdr pair nil))
    (caar tail)))

(defun ednc--path-to-image (image-path)
  "Return image descriptor created from file URI IMAGE-PATH."
  (let ((prefix "file://"))
    (if (and (stringp image-path) (> (length image-path) (length prefix))
             (string-equal (substring image-path 0 (length prefix)) prefix))
        (create-image (substring image-path (length prefix))))))

(defun ednc--data-to-image (image-data)
  "Return image descriptor created from raw (iiibiiay) IMAGE-DATA.

This function is destructive."
  (if image-data
      (cl-destructuring-bind (width height row-stride _ bit-depth channels data)
          image-data
        (when (and (= bit-depth 8) (<= 3 channels 4))
          (ednc--delete-padding data (* channels width) row-stride)
          (ednc--delete-padding data 3 channels)
          (let ((header (format "P6\n%d %d\n255\n" width height)))
            (create-image (apply #'unibyte-string (append header data))
                          'pbm t))))))

(defun ednc--delete-padding (list payload total)
  "Delete LIST elements between multiples of PAYLOAD and TOTAL.

This function is destructive."
  (if (< payload total)
      (let ((cell (cons nil list))
            (delete (if (and (= payload 3) (= total 4)) #'cddr  ; fast opcode
                      (apply-partially #'nthcdr (- total payload -1))))
            (keep (if (= payload 3) #'cdddr (apply-partially #'nthcdr payload))))
        (while (cdr cell)
          (setcdr (setq cell (funcall keep cell)) (funcall delete cell))))))

(defun ednc--push-notification (notification)
  "Push NOTIFICATION to parent state `ednc--state'."
  (let ((state ednc--state))
    (setf (ednc-notification-ednc-tracked notification) state)
    (let ((next (cadr state)))
      (push notification (cdr state))
      (if next (setf (ednc-notification-ednc-tracked next) (cdr state))))))

(defun ednc--delete-notification (notification)
  "Delete NOTIFICATION from parent state and return it."
  (let ((suffix (ednc-notification-ednc-tracked notification)))
    (setf (ednc-notification-ednc-tracked notification) nil)
    (let ((timer (ednc-notification-timer notification)))
      (if timer (cancel-timer timer)))
    (let ((next (caddr suffix)))
      (if next (setf (ednc-notification-ednc-tracked next) suffix)))
    (pop (cdr suffix))))

(defun ednc--dbus-talk-to (service symbol &rest rest)
  "Help with most actions involving D-Bus service SERVICE.

If SERVICE is nil, then a service name is derived from `last-input-event'.

SYMBOL describes a D-Bus function (e.g., `dbus-call-method'),
REST contains the remaining arguments to that function."
  (apply symbol :session (or service (dbus-event-service-name last-input-event))
         ednc--path ednc--interface rest))

(defun ednc--dbus-talk (symbol &rest rest)
  "Help with most actions involving D-Bus service `ednc--service'.

SYMBOL describes a D-Bus function (e.g., `dbus-call-method'),
REST contains the remaining arguments to that function."
  (apply #'ednc--dbus-talk-to ednc--service symbol rest))

(defun ednc-pop-to-notification-in-log-buffer (notification)
  "Pop to NOTIFICATION in its log buffer, if it exists."
  (cl-destructuring-bind (buffer . position)
      (ednc-notification-ednc-logged notification)
    (if (not (buffer-live-p buffer)) (user-error "Log buffer no longer exists")
      (pop-to-buffer buffer)
      (ednc-toggle-body-visibility (goto-char position)))))

(defun ednc--remove-old-notification-from-log-buffer (old)
  "Remove OLD notification from its log buffer, if it exists."
  (cl-destructuring-bind (buffer . position) (ednc-notification-ednc-logged old)
    (if (buffer-live-p buffer)
        (with-current-buffer buffer
          (save-excursion
            (add-text-properties (goto-char position) (line-end-position)
                                 '(face (:strike-through t))))))))

(defun ednc--append-new-notification-to-log-buffer (new)
  "Append NEW notification to log buffer."
  (with-current-buffer (get-buffer-create ednc-log-name)
    (special-mode)
    (use-local-map ednc-log-map)
    (ednc--add-log-controls new)
    (save-excursion (setf (ednc-notification-ednc-logged new)
                          (cons (current-buffer) (goto-char (point-max))))
                    (insert (ednc-format-notification new) ?\n))))

(defun ednc--update-log-buffer (old new)
  "Remove OLD notification from and add NEW one to log buffer."
  (let ((inhibit-read-only t))
    (if old (ednc--remove-old-notification-from-log-buffer old))
    (if new (ednc--append-new-notification-to-log-buffer new))))

(provide 'ednc)
;;; ednc.el ends here
