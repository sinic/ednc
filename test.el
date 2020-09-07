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

;; Load the use cases documented in README.org:
(require 'ob-tangle)
(load-file (car (org-babel-tangle-file "README.org")))

;; Helpers for testing:
(defmacro dnel--with-temp-server (state &rest body)
  (declare (indent 1))
  `(let ((,state (list 0)))
     (skip-unless (not dnel-mode))  ; conflicting servers?
     (dnel--start-server ,state)
     ,@body
     (dnel--test-state-consistency ,state)
     (dnel--stop-server ,state)))

(defconst dnel--default-test-alist
  '((app-name . "test") (replaces-id . 0) (app-icon) (summary . "foo")
    (body . "bar baz")  (actions . ("default" "qux" "other" "quux"))
    (hints) (expire-timeout . 0)))

(defun dnel--get-test-args (&rest override-alist)
  (let ((alist (append override-alist dnel--default-test-alist)))
    (mapcar (lambda (key) (alist-get key alist))
            (mapcar #'car dnel--default-test-alist))))

(defun dnel--test-args-match (state id args)
  (let ((found (cl-find id (cdr state) :key #'dnel-notification-id)))
    (dolist (property (mapcar #'car dnel--default-test-alist))
      (let ((arg (car args)))
        (cond ((eq property 'app-icon))
              ((eq property 'replaces-id)
               (if (zerop arg) (should-not (zerop (dnel-notification-id found)))
                 (should (= arg (dnel-notification-id found)))))
              ((eq property 'expire-timeout)
               (if (zerop arg)
                   (should-not (dnel-notification-timer found))
                 (should (timerp (dnel-notification-timer found)))))
              (t (should (equal arg (cl-struct-slot-value 'dnel-notification
                                                          property found)))))
        (setq args (cdr args))))))

(defun dnel--test-state-consistency (state)
  (let ((distinct (car state))
        (ids (mapcar #'dnel-notification-id (cdr state))))
    (should (and (integerp distinct) (>= distinct 0)))  ; non-negative integer,
    (should (<= (if ids (apply #'max ids) 0) distinct))  ; bounded from above,
    (should (= (length ids) (length (delete-dups ids)))))  ; without duplicates,
  (dolist (notification (cdr state))  ; and with consistent elements?
    (dnel--test-notification-consistency notification)))

(defun dnel--test-notification-consistency (notification)
  (let ((id (dnel-notification-id notification)))
    (should (and (integerp id) (> id 0)))  ; with positive integer and with
    (should (stringp (dnel-notification-summary notification)))  ; required
    (should (stringp (dnel-notification-app-name notification)))))  ; slots?

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

(ert-deftest dnel--test-consistency-of-consistent-state-test ()
  (dolist (arg `((0) (42) (23 ,(dnel--notification-create
                                :id 5 :app-name "test" :summary "foo"))))
    (let ((test (make-ert-test
                 :body (lambda () (dnel--test-state-consistency arg)))))
      (should (ert-test-passed-p (ert-run-test test))))))

(ert-deftest dnel--test-consistency-of-inconsistent-state-test ()
  (dolist (arg `(() (5 (23 ,(dnel--notification-create
                             :app-name "test" :summary "foo")))
                 (,(dnel--notification-create
                    :id 5 :app-name "test" :summary "foo"))
                 (23 ,(dnel--notification-create :id 5 :app-name "test"))))
    (let ((test (make-ert-test
                 :body (lambda () (dnel--test-state-consistency arg)))))
      (should (ert-test-failed-p (ert-run-test test))))))

;; Test use case 1:
(ert-deftest dnel--list-no-notifications-test ()
  (dnel--with-temp-server state
    (should (string-equal "" (list-notifications state)))))

(ert-deftest dnel--list-single-notification-test ()
  (dnel--with-temp-server state
    (apply #'dnel--notify state (dnel--get-test-args))
    (should (string-equal (list-notifications state) "  [test: foo]"))))

(ert-deftest dnel--list-multiple-notifications-test ()
  (dnel--with-temp-server state
    (apply #'dnel--notify state (dnel--get-test-args))
    (apply #'dnel--notify state (dnel--get-test-args '(app-name . "tes1")))
    (should (string-equal (list-notifications state)
                          "  [tes1: foo]  [test: foo]"))))

;; Test use case 2:
(ert-deftest dnel--stack-no-notifications-test ()
  (dnel--with-temp-server state
    (should (string-equal "" (stack-notifications state)))))

(ert-deftest dnel--stack-hidden-notification-test ()
  (dnel--with-temp-server state
    (apply #'dnel--notify state (dnel--get-test-args))
    (should (string-equal "" (stack-notifications state '("test"))))))

(ert-deftest dnel--stack-for-single-notification-test ()
  (dnel--with-temp-server state
    (apply #'dnel--notify state (dnel--get-test-args))
    (should (string-equal (stack-notifications state) "  [test: foo]"))))

(ert-deftest dnel--stack-non-stacking-notifications-test ()
  (dnel--with-temp-server state
    (apply #'dnel--notify state (dnel--get-test-args))
    (apply #'dnel--notify state (dnel--get-test-args '(app-name . "tes1")))
    (should (string-equal (stack-notifications state)
                          "  [tes1: foo]  [test: foo]"))))

(ert-deftest dnel--stack-stacking-notifications-test ()
  (dnel--with-temp-server state
    (apply #'dnel--notify state (dnel--get-test-args))
    (apply #'dnel--notify state (dnel--get-test-args '(summary . "bar")))
    (should (string-equal (stack-notifications state) "  [test: bar]"))))

;; Test logging:
(ert-deftest dnel--log-single-notification-test ()
  (dnel--with-temp-server state
    (apply #'dnel--notify state (dnel--get-test-args))
    (with-temp-buffer
      (dnel--update-log state (cadr state))
      (should (string-equal (buffer-string) "  [test: foo]
bar baz

")))))

(ert-deftest dnel--log-multiple-notifications-test ()
    (dnel--with-temp-server state
      (apply #'dnel--notify state (dnel--get-test-args))
      (with-temp-buffer
        (dnel--update-log state (cadr state))
        (apply #'dnel--notify state (dnel--get-test-args '(app-name . "tes1")))
        (dnel--update-log state (cadr state))
        (should (string-equal (buffer-string) "  [test: foo]
bar baz

  [tes1: foo]
bar baz

")))))

(ert-deftest dnel--log-closed-notifications-test ()
  (dnel--with-temp-server state
    (let* ((id (apply #'dnel--notify state (dnel--get-test-args)))
           (notification (cl-find id (cdr state) :key #'dnel-notification-id)))
      (with-temp-buffer
        (dnel--update-log state notification)
        (apply #'dnel--notify state (dnel--get-test-args '(app-name . "tes1")))
        (dnel--update-log state (cadr state))
        (dnel-close-notification (cadr state) 3)
        (dnel--update-log state notification t)
        (should (string-equal (buffer-string) "  [test: foo]
bar baz

  [tes1: foo]
bar baz

"))))))

;; Test dnel-invoke-action:
(ert-deftest dnel--invoke-default-action-test ()
  (dnel--with-temp-server state
    (apply #'dnel--notify state (dnel--get-test-args))
    (dnel-invoke-action (cadr state))))  ; no real test yet

(ert-deftest dnel--invoke-alternative-action-test ()
  (dnel--with-temp-server state
    (apply #'dnel--notify state (dnel--get-test-args))
    (dnel-invoke-action (cadr state) "other")))  ; no real test yet

;; Test dnel-close-notification:
(ert-deftest dnel--close-notification-test ()
  (dnel--with-temp-server state
    (let ((id (apply #'dnel--notify state (dnel--get-test-args))))
      (dnel-close-notification (cadr state) 3)
      (should-not (cl-find id (cdr state) :key #'dnel-notification-id)))))

;; Test dnel--format-notification:
(ert-deftest dnel--format-notification-test ()
  (dnel--with-temp-server state
    (apply #'dnel--notify state (dnel--get-test-args))
    (should (string-equal (dnel-format-notification state (cadr state))
                          "  [test: foo]"))))

;; Test dnel--format-summary:
(ert-deftest dnel--format-summary-test ()
  (dnel--with-temp-server state
    (apply #'dnel--notify state (dnel--get-test-args))
    (let* ((new (cadr state))
           (result (dnel--format-summary state new)))
      (should (string-equal result (dnel-notification-summary new))))))

;; Test dnel--get-actions-keymap:
(ert-deftest dnel--get-empty-actions-keymap-test ()
  (dnel--with-temp-server state
    (apply #'dnel--notify state (dnel--get-test-args '(actions)))
    (let* ((new (cadr state))
           (result (dnel--get-actions-keymap new)))
      (should (eq (car result) 'keymap))
      (should (string-equal (cadr result) "Actions")))))

(ert-deftest dnel--get-actions-keymap-test ()
  (dnel--with-temp-server state
    (apply #'dnel--notify state (dnel--get-test-args))
    (let* ((new (cadr state))
           (result (dnel--get-actions-keymap new)))
      (should (eq (car result) 'keymap))
      (dotimes (i 2)
        (let ((entry (cadr result)))
          (should (and (= (car entry) i) (eq (cadr entry) 'menu-item)))
          (should (string-equal (caddr entry)
                                (nth (1+ (* i 2))  ; odd elements
                                     (dnel-notification-actions new))))
          (should (functionp (cadddr entry))))
        (setq result (cdr result)))
      (should (string-equal (cadr result) "Actions")))))

;; Test handler of Notify:
(ert-deftest dnel--handle-notify-test ()
  (dnel--with-temp-server state
    (let ((id (apply #'dnel--dbus-talk 'call-method 'Notify (dnel--get-test-args))))
      (dnel--test-args-match state id (dnel--get-test-args)))))

(ert-deftest dnel--handle-notify-with-expiration-time-test ()
  (dnel--with-temp-server state
    (let* ((timeout 5)
           (args (dnel--get-test-args `(expire-timeout . ,timeout)))
           (id (apply #'dnel--dbus-talk 'call-method 'Notify args)))
      (dnel--test-args-match state id args)
      (sleep-for (/ (* 2 timeout) 1000.0))
      (should-not (cl-find id (cdr state) :key #'dnel-notification-id)))))

(ert-deftest dnel--handle-notify-replace-test ()
  (dnel--with-temp-server state
    (let* ((id (apply #'dnel--notify state (dnel--get-test-args)))
           (args (dnel--get-test-args `(replaces-id . ,id))))
      (apply #'dnel--dbus-talk 'call-method 'Notify args)
      (dnel--test-args-match state id args))))

(ert-deftest dnel--handle-notify-replace-nonexistent-test ()
  (dnel--with-temp-server state
    (let* ((id (apply #'dnel--notify state (dnel--get-test-args)))
           (args (dnel--get-test-args `(replaces-id . ,(+ 5 id)))))
      (dnel--test-args-match state id (dnel--get-test-args))
      (setq id (apply #'dnel--dbus-talk 'call-method 'Notify args))
      (dnel--test-args-match state id (dnel--get-test-args)))))

;; Test handler of CloseNotification:
(ert-deftest dnel--handle-close-notification-test ()
  (dnel--with-temp-server state
    (let ((id (apply #'dnel--notify state (dnel--get-test-args))))
      (dnel--dbus-talk 'call-method 'CloseNotification id)
      (should-not (cl-find id (cdr state) :key #'dnel-notification-id)))))

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

;; Test dnel--get-hint:
(ert-deftest dnel--get-hint-test ()
  (let ((hints '(("foo" (("bar" "baz"))) ("qux" ("quux")))))
    (should (equal '("bar" "baz") (dnel--get-hint hints "foo")))
    (should (string-equal "quux" (dnel--get-hint hints "qux")))))

(ert-deftest dnel--get-and-remove-hint-test ()
  (let ((hints (list (list "foo" '(("bar" "baz"))) (list "qux" '("quux")))))
    (should (equal '("bar" "baz") (dnel--get-hint hints "foo" t)))
    (should-not (dnel--get-hint hints "foo"))  ; older gone, and
    (should (string-equal "quux" (dnel--get-hint hints "qux" t)))
    (should-not (dnel--get-hint hints "qux"))))  ; newest gone?

(ert-deftest dnel--get-or-remove-nonexistent-hint-test ()
  (let ((hints nil))
    (should-not (dnel--get-hint hints "foo" t))  ; neither remove,
    (should-not (dnel--get-hint hints "foo"))))  ; nor get only?

;; Test dnel--path-to-image and dnel--data-to-image:
(ert-deftest dnel--nil-to-image-test ()
  (should-not (dnel--path-to-image nil))
  (should-not (dnel--data-to-image nil)))

(ert-deftest dnel--test-unsupported-paths-to-image ()
  (should-not (dnel--path-to-image "/ne"))  ; no schema, shorter than "file://",
  (should-not (dnel--path-to-image "/nonexistent"))  ; and longer than "file://"
  (should-not (dnel--path-to-image
               "https://www.gnu.org/software/emacs/images/emacs.png")))

(ert-deftest dnel--nonexistent-path-to-image-test ()
  (should-not (dnel--path-to-image "file:///nonexistent")))

(ert-deftest dnel--unsupported-data-to-image-test ()
  (let ((raw (append "abcABCxyzXYZ" nil)))
    (should-not (dnel--data-to-image (list 2 2 6 t 7 3 raw)))  ; bit-depth
    (should-not (dnel--data-to-image (list 2 2 6 t 8 2 raw)))  ; non-RGB(A)
    (should-not (dnel--data-to-image (list 2 2 6 t 8 5 raw)))))

(ert-deftest dnel--data-to-image-test ()
  (let* ((expect "P6\n2 2\n255\nabcABCxyzXYZ")
         (data (append "abcABCxyzXYZ" nil))
         (image (dnel--data-to-image (list 2 2 6 nil 8 3 data))))
    (should (equal (image-property image :data) expect))  ; RGB
    (setq data (append "abc!ABC?xyz?XYZ!" nil)
          image (dnel--data-to-image (list 2 2 8 t 8 4 data)))
    (should (equal (image-property image :data) expect))))  ; RGBA

;; Test dnel--delete-padding:
(ert-deftest dnel--delete-padding-from-empty-list-test ()
  (let ((list (list)))
    (dnel--delete-padding list 3 5)
    (should (null list))))

(ert-deftest dnel--delete-zero-length-padding-from-list-test ()
  (let ((list (list 'foo 'bar 'baz 'qux 'quux 'corge)))
    (dnel--delete-padding list 3 3)
    (should (equal list '(foo bar baz qux quux corge)))))

(ert-deftest dnel--delete-padding-from-short-list-test ()
  (let ((list (list 'foo 'bar 'baz 'qux 'quux)))
    (dnel--delete-padding list 3 5)
    (should (equal list '(foo bar baz)))))

(ert-deftest dnel--delete-alpha-channel-from-short-list-test ()
  (let ((list (list 'foo 'bar 'baz 'qux)))
    (dnel--delete-padding list 3 4)
    (should (equal list '(foo bar baz)))))

(ert-deftest dnel--delete-padding-from-longer-list-test ()
    (let ((list (list 'foo 'bar 'baz 'qux 'quux 'corge)))
      (dnel--delete-padding list 1 3)
      (should (equal list '(foo qux)))))

(ert-deftest dnel--delete-alpha-channel-from-longer-list-test ()
  (let ((list (list 'foo 'bar 'baz 'qux 'quux 'corge 'grault 'garply)))
    (dnel--delete-padding list 3 4)
    (should (equal list '(foo bar baz quux corge grault)))))

;;; test.el ends here
