DNel is an Emacs package that implements a
[Desktop Notifications server](https://people.gnome.org/~mccann/docs/notification-spec/notification-spec-latest.html)
in pure Lisp, aspiring to be a small, but flexible drop-in replacement for
standalone daemons like [Dunst](https://dunst-project.org/).
Active notifications are tracked in the global variable `dnel-state`
whenever the global minor mode `dnel-mode` is active.
Users are free to monitor the contents of that variable as they see fit, though
DNel does provide some additional convenience functions.

# Getting Started

For now, simply execute the `package-install-file` command and point it to a
local copy of this repository.
The package will hopefully end up on [MELPA](https://melpa.org) in due time.
After installation, ensure that no other notification daemon is active, execute
`dnel-mode`, and make use of your session's currently active notifications in
`dnel-state` whenever the hook `dnel-state-changed-hook` runs (read on for some
suggestions).
Enjoy!

## Use Case 1: Show Active Notifications

Simply showing all active notifications in the mode-line could be accomplished
in the following way:
```elisp
(defun format-notifications (notifications)
  (mapconcat (lambda (notification)
               (dnel-format-notification notification notifications))
             (cdr notifications) ""))

(nconc global-mode-string '((:eval (format-notifications dnel-state))))
(add-hook 'dnel-state-changed-hook
          (lambda () (force-mode-line-update t)))
```

Every notification has text properties so that
* hovering over its summary displays the body as a tooltip,
* the left mouse button invokes the notification's default action,
* the middle mouse button shows a menu of all available actions, and
* the right mouse button closes the notification.

## Use Case 2: Stack Active Notifications by Application

Instead of showing all notifications, it is possible to only display the newest
notification per application in the following way:
```elisp
(defun format-notifications (notifications &optional hide)
  (mapconcat (lambda (notification)
               (let ((app-name (plist-get (cdr notification) 'app-name)))
                 (if (member app-name hide) ""
                   (push app-name hide)
                   (dnel-format-notification notification notifications))))
             (cdr notifications) ""))

(nconc global-mode-string '((:eval (format-notifications dnel-state))))
(add-hook 'dnel-state-changed-hook
          (lambda () (force-mode-line-update t)))
```

## Use Case 3: Log All Notifications

Instead of or in addition to one of the previous use cases, a log of past
and present notifications can be kept in the following way:
```elisp
(defun update-notification-log (notifications)
  (goto-char (point-min))
  (let ((copy (copy-sequence notifications)))
    (while (not (eobp))
      (unless (equal (get-text-property (point) 'face) '(:inherit shadow))
        (let* ((line (delete-and-extract-region (point) (line-end-position)))
               (found (dnel-get-notification (string-to-number line) copy t)))
          (insert (if found (dnel-format-notification found notifications)
                    (propertize line 'face '(:inherit shadow) 'local-map ())))))
      (forward-line))
    (dolist (new (cdr copy))
      (insert (dnel-format-notification new notifications) ?\n))))

(add-hook 'dnel-state-changed-hook
          (lambda () (with-current-buffer (get-buffer-create "*Notifications*")
                       (save-excursion
                         (update-notification-log dnel-state)))))
```

Active notifications have text properties as described above; closed or expired
notifications only have the tooltip and are grayed out.
