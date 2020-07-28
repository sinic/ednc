This package implements a
[Desktop Notifications server](https://people.gnome.org/~mccann/docs/notification-spec/notification-spec-latest.html)
in Emacs Lisp, aiming to be a drop-in replacement for standalone daemons like
[Dunst](https://dunst-project.org/).
Active notifications are tracked in the variable `dnel-notifications` whenever
the global minor mode `dnel-mode` is active.
A convenience function `dnel-get-default-propertized-string` is provided,
suitable for inclusion in the user's mode-line, for example like this:

    (nconc global-mode-string '((:eval (dnel-get-default-propertized-string
                                        dnel-notifications))))
    (add-hook 'dnel-notifications-changed-hook
              (lambda () (force-mode-line-update t)))
