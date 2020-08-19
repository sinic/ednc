;;; test.el --- DNel tests -*- lexical-binding: t; -*-
;; Copyright (C) 2020 Simon Nicolussi

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
(require 'dnel)

;; Use cases from README:
(defun dnel--format-notifications-1 (notifications)
  (mapconcat (lambda (notification)
               (dnel-format-notification notification notifications))
             (cdr notifications) ""))

(defun dnel--format-notifications-2 (notifications &optional hide)
  (mapconcat (lambda (notification)
               (let ((app-name (plist-get (cdr notification) 'app-name)))
                 (if (member app-name hide) ""
                   (push app-name hide)
                   (dnel-format-notification notification notifications))))
             (cdr notifications) ""))

(defun dnel--format-notifications-3 (notifications)
  (goto-char (point-min))
  (let ((copy (copy-sequence notifications))
        (closed '(:strike-through t)))
    (while (not (eobp))
      (unless (equal (get-text-property (point) 'face) closed)
        (let* ((line (delete-and-extract-region (point) (line-end-position)))
               (found (dnel-get-notification (string-to-number line) copy t)))
          (insert (if found (dnel-format-notification found notifications)
                    (propertize line 'face closed 'local-map ())))))
      (forward-line))
    (dolist (new (cdr copy))
      (insert (dnel-format-notification new notifications) ?\n))))

;; Helpers for testing:
(defmacro dnel--with-temp-server (notifications &rest body)
  (declare (indent 1))
  `(let ((,notifications (list 0)))
     (skip-unless (not dnel-mode))  ; conflicting servers?
     (dnel--start-server ,notifications)
     ,@body
     (dnel--test-notifications-consistency ,notifications)
     (dnel--stop-server ,notifications)))

(defconst dnel--default-test-alist
  '((app-name . "test") (replaces-id . 0) (app-icon) (summary . "foo")
    (body . "bar baz")  (actions . ("default" "qux" "other" "quux"))
    (hints) (expire-timeout . 0)))

(defun dnel--get-test-args (&rest override-alist)
  (let ((alist (append override-alist dnel--default-test-alist)))
    (mapcar (lambda (key) (alist-get key alist))
            (mapcar #'car dnel--default-test-alist))))

(defun dnel--test-args-match (state id args)
  (let ((notification (dnel-get-notification id state)))
    (dolist (property (mapcar #'car dnel--default-test-alist))
      (let ((arg (car args))
            (plist (cdr notification)))
        (cond ((eq property 'replaces-id)
               (if (zerop arg) (should-not (zerop (car notification)))
                 (should (= arg (car notification)))))
              ((eq property 'expire-timeout)
               (if (zerop arg) (should-not (plist-get plist 'timer))
                 (should (timerp (plist-get plist 'timer)))))
              (t (should (equal arg (plist-get plist property)))))
        (setq args (cdr args))))))

(defun dnel--test-notifications-consistency (notifications)
  (let ((distinct (car notifications))
        (ids (mapcar #'car (cdr notifications))))
    (should (and (integerp distinct) (>= distinct 0)))  ; non-negative integer,
    (should (<= (if ids (apply #'max ids) 0) distinct))  ; bounded from above,
    (should (= (length ids) (length (delete-dups ids)))))  ; without duplicates,
  (dolist (notification (cdr notifications))  ; and with consistent elements?
    (dnel--test-notification-consistency notification)))

(defun dnel--test-notification-consistency (notification)
  (let ((id (car notification)))
    (should (and (integerp id) (> id 0)))  ; positive integer and
    (dolist (required '(app-name summary))  ; plist with required properties?
      (should (stringp (plist-get (cdr notification) required))))))

;; Test helpers for testing:
(ert-deftest dnel--with-temporary-server-test ()
  (dnel--with-temp-server state))  ; no real test yet

(ert-deftest dnel--get-default-test-arguments-test ()
  (let ((args (dnel--get-test-args)))
    (dolist (expected (mapcar #'cdr dnel--default-test-alist))
      (should (equal (car args) expected))  ; with all the default arguments?
      (setq args (cdr args)))))

(ert-deftest dnel--get-overridden-test-arguments-test ()
  (let ((args (dnel--get-test-args '(app-name . "tes1") '(replaces-id . 42))))
    (should (equal (car args) "tes1"))  ; with overridden string argument,
    (should (equal (cadr args) 42))  ; overridden integer argument,
    (setq args (cddr args))
    (dolist (expected (mapcar #'cdr (cddr dnel--default-test-alist)))
      (should (equal (car args) expected))  ; and default arguments otherwise?
      (setq args (cdr args)))))

(ert-deftest dnel--test-match-of-matching-arguments-test ()
  (dnel--with-temp-server state
    (let* ((args (dnel--get-test-args '(body . "baz bar")))
           (id (apply #'dnel--notify state args))
           (test (make-ert-test
                  :body (lambda () (dnel--test-args-match state id args)))))
      (should (ert-test-passed-p (ert-run-test test))))))

(ert-deftest dnel--test-match-of-mismatching-arguments-test ()
  (dnel--with-temp-server state
    (let* ((args (dnel--get-test-args '(body . "baz bar")))
           (id (apply #'dnel--notify state (dnel--get-test-args)))
           (test (make-ert-test
                  :body (lambda () (dnel--test-args-match state id args)))))
      (should-error (ert-test-passed-p (ert-run-test test))))))

(ert-deftest dnel--test-consistency-of-consistent-notifications-test ()
  (dolist (arg '((0) (42) (23 (5 app-name "test" summary "foo"))))
    (let ((test (make-ert-test
                 :body (lambda () (dnel--test-notifications-consistency arg)))))
      (should (ert-test-passed-p (ert-run-test test))))))

(ert-deftest dnel--test-consistency-of-inconsistent-notifications-test ()
  (dolist (arg '(() (5 (23 app-name "test" summary "foo"))
                 ((5 app-name "test" summary "foo")) (23 (5 app-name "test"))))
    (let ((test (make-ert-test
                 :body (lambda () (dnel--test-notifications-consistency arg)))))
      (should (ert-test-failed-p (ert-run-test test))))))

;; Test use case 1:
(ert-deftest dnel--format-1-string-for-no-notifications-test ()
  (dnel--with-temp-server state
    (should (string-equal "" (dnel--format-notifications-1 state)))))

(ert-deftest dnel--format-1-string-for-single-notification-test ()
  (dnel--with-temp-server state
    (apply #'dnel--notify state (dnel--get-test-args))
    (should (string-equal (dnel--format-notifications-1 state)
                          "1 [test: foo]"))))

(ert-deftest dnel--format-1-string-for-multiple-notifications-test ()
  (dnel--with-temp-server state
    (apply #'dnel--notify state (dnel--get-test-args))
    (apply #'dnel--notify state (dnel--get-test-args '(app-name . "tes1")))
    (should (string-equal (dnel--format-notifications-1 state)
                          "2 [tes1: foo]1 [test: foo]"))))

;; Test use case 2:
(ert-deftest dnel--format-2-string-for-no-notifications-test ()
  (dnel--with-temp-server state
    (should (string-equal "" (dnel--format-notifications-2 state)))))

(ert-deftest dnel--format-2-string-for-hidden-notification-test ()
  (dnel--with-temp-server state
    (apply #'dnel--notify state (dnel--get-test-args))
    (should (string-equal ""
             (dnel--format-notifications-2 state '("test"))))))

(ert-deftest dnel--format-2-string-for-single-notification-test ()
  (dnel--with-temp-server state
    (apply #'dnel--notify state (dnel--get-test-args))
    (should (string-equal (dnel--format-notifications-2 state)
                          "1 [test: foo]"))))

(ert-deftest dnel--format-2-string-for-non-stacking-notifications-test ()
  (dnel--with-temp-server state
    (apply #'dnel--notify state (dnel--get-test-args))
    (apply #'dnel--notify state (dnel--get-test-args '(app-name . "tes1")))
    (should (string-equal (dnel--format-notifications-2 state)
                          "2 [tes1: foo]1 [test: foo]"))))

(ert-deftest dnel--propertize-string-for-stacking-notifications-test ()
  (dnel--with-temp-server state
    (apply #'dnel--notify state (dnel--get-test-args))
    (apply #'dnel--notify state (dnel--get-test-args '(summary . "bar")))
    (should (string-equal (dnel--format-notifications-2 state)
                          "2 [test: bar]"))))

;; Test use case 3:
(ert-deftest dnel--format-3-string-for-no-notifications-test ()
  (dnel--with-temp-server state
    (with-temp-buffer
      (dnel--format-notifications-3 state)
      (should (string-equal (buffer-string) "")))))

(ert-deftest dnel--format-3-string-for-single-notification-test ()
  (dnel--with-temp-server state
    (apply #'dnel--notify state (dnel--get-test-args))
    (with-temp-buffer
      (dnel--format-notifications-3 state)
      (should (string-equal (buffer-string) "1 [test: foo]
")))))

(ert-deftest dnel--format-3-string-for-multiple-notifications-test ()
    (dnel--with-temp-server state
      (apply #'dnel--notify state (dnel--get-test-args))
      (with-temp-buffer
        (dnel--format-notifications-3 state)
        (apply #'dnel--notify state (dnel--get-test-args '(app-name . "tes1")))
        (dnel--format-notifications-3 state)
        (should (string-equal (buffer-string) "1 [test: foo]
2 [tes1: foo]
")))))

(ert-deftest dnel--format-3-string-for-shadowed-notifications-test ()
  (dnel--with-temp-server state
    (let ((id (apply #'dnel--notify state (dnel--get-test-args))))
      (with-temp-buffer
        (dnel--format-notifications-3 state)
        (apply #'dnel--notify state (dnel--get-test-args '(app-name . "tes1")))
        (dnel--format-notifications-3 state)
        (dnel-close-notification state id 3)
        (dnel--format-notifications-3 state)
        (should (string-equal (buffer-string) "1 [test: foo]
2 [tes1: foo]
"))))))

;; Test dnel-invoke-action:
(ert-deftest dnel--invoke-action-on-nonexistent-notification-test ()
  (dnel--with-temp-server state
    (let ((unused (1+ (apply #'dnel--notify state (dnel--get-test-args)))))
      (dnel-invoke-action state unused))))  ; no real test yet

(ert-deftest dnel--invoke-default-action-test ()
  (dnel--with-temp-server state
    (let ((id (apply #'dnel--notify state (dnel--get-test-args))))
      (dnel-invoke-action state id))))  ; no real test yet

(ert-deftest dnel--invoke-alternative-action-test ()
  (dnel--with-temp-server state
    (let ((id (apply #'dnel--notify state (dnel--get-test-args))))
      (dnel-invoke-action state id "other"))))  ; no real test yet

;; Test dnel-close-notification:
(ert-deftest dnel--close-notification-test ()
  (dnel--with-temp-server state
    (let ((id (apply #'dnel--notify state (dnel--get-test-args))))
      (should (eq (dnel-close-notification state id 3) :ignore))
      (should-not (dnel-get-notification id state)))))  ; gone?

(ert-deftest dnel--close-previously-closed-notification-test ()
  (dnel--with-temp-server state
    (let ((id (apply #'dnel--notify state (dnel--get-test-args))))
      (dnel-close-notification state id)
      (should (eq (dnel-close-notification state id 2) :ignore))
      (should-error (dnel-close-notification state id)))))  ; if by handler

(ert-deftest dnel--close-nonexistent-notification-test ()
  (dnel--with-temp-server state
    (let ((unused (1+ (apply #'dnel--notify state (dnel--get-test-args)))))
      (should (eq (dnel-close-notification state unused 2) :ignore))
      (should-error (dnel-close-notification state unused)))))  ; if by handler

;; Test dnel--format-notification:
(ert-deftest dnel--format-notification-test ()
  (dnel--with-temp-server state
    (apply #'dnel--notify state (dnel--get-test-args))
    (should (string-equal (dnel-format-notification (cadr state) state)
                          "1 [test: foo]"))))

;; Test dnel--format-summary:
(ert-deftest dnel--format-summary-test ()
  (dnel--with-temp-server state
    (let* ((id (apply #'dnel--notify state (dnel--get-test-args)))
           (plist (cdadr state))
           (result (dnel--format-summary (plist-get plist 'summary) id
                                         state (plist-get plist 'body))))
      (should (string-equal result (plist-get plist 'summary)))
      (should (string-equal (get-text-property 0 'help-echo result)
                            (plist-get plist 'body))))))

(ert-deftest dnel--format-summary-with-empty-body-test ()
  (dnel--with-temp-server state
    (let* ((id (apply #'dnel--notify state (dnel--get-test-args '(body . ""))))
           (plist (cdadr state))
           (result (dnel--format-summary (plist-get plist 'summary)
                                         id state "")))
      (should (string-equal result (plist-get plist 'summary)))
      (should-not (get-text-property 0 'help-echo result)))))

(ert-deftest dnel--format-summary-without-body-test ()
  (dnel--with-temp-server state
    (let* ((id (apply #'dnel--notify state (dnel--get-test-args '(body))))
           (plist (cdadr state))
           (result (dnel--format-summary (plist-get plist 'summary) id state)))
      (should (string-equal result (plist-get plist 'summary)))
      (should-not (get-text-property 0 'help-echo result)))))

;; Test dnel--format-actions:
(ert-deftest dnel--format-empty-actions-test ()
  (dnel--with-temp-server state
    (let* ((id (apply #'dnel--notify state (dnel--get-test-args '(actions))))
           (plist (cdadr state))
           (result (dnel--format-actions (plist-get plist 'actions) id state)))
      (should (eq (car result) 'keymap))
      (should (string-equal (cadr result) "Actions")))))

(ert-deftest dnel--format-actions-test ()
  (dnel--with-temp-server state
    (let* ((id (apply #'dnel--notify state (dnel--get-test-args)))
           (plist (cdadr state))
           (result (dnel--format-actions (plist-get plist 'actions) id state)))
      (should (eq (car result) 'keymap))
      (dotimes (i 2)
        (let ((entry (cadr result)))
          (should (and (= (car entry) i) (eq (cadr entry) 'menu-item)))
          (should (string-equal (caddr entry) (nth (1+ (* i 2))  ; odd elements
                                                   (plist-get plist 'actions))))
          (should (functionp (cadddr entry))))
        (setq result (cdr result)))
      (should (string-equal (cadr result) "Actions")))))

;; Test dnel--handle-Notify:
(ert-deftest dnel--handle-notify-test ()
  (dnel--with-temp-server state
    (let ((id (apply #'dnel--dbus-talk 'call-method 'Notify (dnel--get-test-args))))
      (dnel--test-args-match state id (dnel--get-test-args)))))

(ert-deftest dnel--handle-notify-with-expiration-time-test ()
  (dnel--with-temp-server state
    (let* ((args (dnel--get-test-args '(expire-timeout . 5)))
           (id (apply #'dnel--dbus-talk 'call-method 'Notify args)))
      (dnel--test-args-match state id args))))

(ert-deftest dnel--handle-notify-replace-test ()
  (dnel--with-temp-server state
    (let* ((id (apply #'dnel--notify state (dnel--get-test-args)))
           (args (dnel--get-test-args `(replaces-id . ,id))))
      (apply #'dnel--dbus-talk 'call-method 'Notify args)
      (dnel--test-args-match state id args))))

;; Test dnel--handle-CloseNotification:
(ert-deftest dnel--handle-close-notification-test ()
  (dnel--with-temp-server state
    (let ((id (apply #'dnel--notify state (dnel--get-test-args))))
      (dnel--dbus-talk 'call-method 'CloseNotification id)
      (should-not (dnel-get-notification id state)))))  ; gone?

(ert-deftest dnel--handle-close-previously-closed-notification-test ()
  (dnel--with-temp-server state
    (let ((id (apply #'dnel--notify state (dnel--get-test-args))))
      (dnel--dbus-talk 'call-method 'CloseNotification id)
      (should-error (dnel--dbus-talk 'call-method 'CloseNotification id)))))

(ert-deftest dnel--handle-close-nonexistent-notification-test ()
  (dnel--with-temp-server state
    (let ((unused (1+ (apply #'dnel--notify state (dnel--get-test-args)))))
      (should-error (dnel--dbus-talk 'call-method 'CloseNotification unused)))))

;; Test informational handlers:
(ert-deftest dnel--handle-get-server-information-test ()
  (dnel--with-temp-server state
    (let ((info (dnel--dbus-talk 'call-method 'GetServerInformation)))
      (should (and (listp info) (= (length info) 4)))  ; correct aggregate type
      (dolist (field info)
        (should (stringp field))))))  ; and correct types in aggregate?

(ert-deftest dnel--handle-get-capabilities-test ()
  (dnel--with-temp-server state
    (let ((capabilities (dnel--dbus-talk 'call-method 'GetCapabilities)))
      (should (listp capabilities))  ; correct aggregate type?
      (dolist (capability capabilities)
        (should (stringp capability)))  ; correct types in aggregate,
      (dolist (required '("actions" "body"))  ; and minimal feature set?
        (should (member required capabilities))))))

;; Test dnel-get-notification:
(ert-deftest dnel-get-notification-test ()
  (dnel--with-temp-server state
    (apply #'dnel--notify state (dnel--get-test-args))
    (let* ((older (apply #'dnel--notify state (dnel--get-test-args)))
           (newest (apply #'dnel--notify state (dnel--get-test-args))))
      (should (= older (car (dnel-get-notification older state))))
      (should (= newest (car (dnel-get-notification newest state)))))))

(ert-deftest dnel--get-and-remove-notification-test ()
  (dnel--with-temp-server state
    (apply #'dnel--notify state (dnel--get-test-args))
    (let* ((older (apply #'dnel--notify state (dnel--get-test-args)))
           (newest (apply #'dnel--notify state (dnel--get-test-args))))
      (should (= older (car (dnel-get-notification older state t))))
      (should-not (dnel-get-notification older state))  ; older gone, and
      (should (= newest (car (dnel-get-notification newest state t))))
      (should-not (dnel-get-notification newest state)))))  ; newest gone?

(ert-deftest dnel--get-or-remove-nonexistent-notification-test ()
  (dnel--with-temp-server state
    (let ((unused (1+ (apply #'dnel--notify state (dnel--get-test-args)))))
      (should-not (dnel-get-notification unused state t))  ; neither remove,
      (should-not (dnel-get-notification unused state)))))  ; nor get only?

;;; test.el ends here
