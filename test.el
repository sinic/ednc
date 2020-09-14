;;; test.el --- EDNC tests -*- lexical-binding: t; -*-
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
(require 'ednc)

;; Load the use cases documented in README.org:
(require 'ob-tangle)
(load-file (car (org-babel-tangle-file "README.org")))

;; Helpers for testing:
(defmacro ednc--with-temp-server (&rest body)
  (declare (indent defun))
  `(let ((ednc-log-name "*ednc-test-log*")
         (ednc-notifications-changed-functions)
         (ednc--state (list 0)))
     (skip-unless (not ednc-mode))  ; conflicting servers?
     (ednc--start-server)
     ,@body
     (ednc--test-state-consistency)
     (ednc--stop-server)
     (if (get-buffer ednc-log-name)
         (kill-buffer ednc-log-name))))

(defconst ednc--default-test-alist
  '((app-name . "test") (replaces-id . 0) (app-icon) (summary . "foo")
    (body . "bar baz")  (actions . ("default" "qux" "other" "quux"))
    (hints) (expire-timeout . 0)))

(defun ednc--get-test-args (&rest override-alist)
  (let ((alist (append override-alist ednc--default-test-alist)))
    (mapcar (lambda (key) (alist-get key alist))
            (mapcar #'car ednc--default-test-alist))))

(defun ednc--test-args-match (id args)
  (let ((found (cl-find id (cdr ednc--state) :key #'ednc-notification-id)))
    (dolist (property (mapcar #'car ednc--default-test-alist))
      (let ((arg (car args)))
        (cond ((eq property 'app-icon))
              ((eq property 'replaces-id)
               (if (zerop arg) (should-not (zerop (ednc-notification-id found)))
                 (should (= arg (ednc-notification-id found)))))
              ((eq property 'expire-timeout)
               (if (zerop arg)
                   (should-not (ednc-notification-timer found))
                 (should (timerp (ednc-notification-timer found)))))
              (t (should (equal arg (cl-struct-slot-value 'ednc-notification
                                                          property found)))))
        (setq args (cdr args))))))

(defun ednc--test-state-consistency ()
  (let ((distinct (car ednc--state))
        (ids (mapcar #'ednc-notification-id (cdr ednc--state))))
    (should (and (integerp distinct) (>= distinct 0)))  ; non-negative integer,
    (should (<= (if ids (apply #'max ids) 0) distinct))  ; bounded from above,
    (should (= (length ids) (length (delete-dups ids)))))  ; without duplicates,
  (dolist (notification (cdr ednc--state))  ; and with consistent elements?
    (ednc--test-notification-consistency notification)))

(defun ednc--test-notification-consistency (notification)
  (let ((id (ednc-notification-id notification)))
    (should (and (integerp id) (> id 0)))  ; with positive integer and with
    (should (stringp (ednc-notification-summary notification)))  ; required
    (should (stringp (ednc-notification-app-name notification)))))  ; slots?

;; Test helpers for testing:
(ert-deftest ednc--with-temporary-server-test ()
  (ednc--with-temp-server))  ; no real test yet

(ert-deftest ednc--get-default-test-arguments-test ()
  (let ((args (ednc--get-test-args)))
    (dolist (expected (mapcar #'cdr ednc--default-test-alist))
      (should (equal (car args) expected))  ; with all the default arguments?
      (setq args (cdr args)))))

(ert-deftest ednc--get-overridden-test-arguments-test ()
  (let ((args (ednc--get-test-args '(app-name . "tes1") '(replaces-id . 42))))
    (should (equal (car args) "tes1"))  ; with overridden string argument,
    (should (equal (cadr args) 42))  ; overridden integer argument,
    (setq args (cddr args))
    (dolist (expected (mapcar #'cdr (cddr ednc--default-test-alist)))
      (should (equal (car args) expected))  ; and default arguments otherwise?
      (setq args (cdr args)))))

(ert-deftest ednc--test-match-of-matching-arguments-test ()
  (ednc--with-temp-server
    (let* ((args (ednc--get-test-args '(body . "baz bar")))
           (id (apply #'ednc--notify args))
           (test (make-ert-test
                  :body (lambda () (ednc--test-args-match id args)))))
      (should (ert-test-passed-p (ert-run-test test))))))

(ert-deftest ednc--test-match-of-mismatching-arguments-test ()
  (ednc--with-temp-server
    (let* ((args (ednc--get-test-args '(body . "baz bar")))
           (id (apply #'ednc--notify (ednc--get-test-args)))
           (test (make-ert-test
                  :body (lambda () (ednc--test-args-match id args)))))
      (should-error (ert-test-passed-p (ert-run-test test))))))

(ert-deftest ednc--test-consistency-of-consistent-state-test ()
  (dolist (ednc--state `((0) (42) (23 ,(ednc--notification-create
                                        :id 5 :app-name "foo" :summary "bar"))))
    (let ((test (make-ert-test :body #'ednc--test-state-consistency)))
      (should (ert-test-passed-p (ert-run-test test))))))

(ert-deftest ednc--test-consistency-of-inconsistent-state-test ()
  (dolist (ednc--state `(() (5 (23 ,(ednc--notification-create
                                     :app-name "foo" :summary "bar")))
                         (,(ednc--notification-create
                            :id 5 :app-name "foo" :summary "bar"))
                         (23 ,(ednc--notification-create
                               :id 5 :app-name "foo"))))
    (let ((test (make-ert-test :body #'ednc--test-state-consistency)))
      (should (ert-test-failed-p (ert-run-test test))))))

;; Test use case 1:
(ert-deftest ednc--list-no-notifications-test ()
  (ednc--with-temp-server
    (should (string-equal "" (list-notifications)))))

(ert-deftest ednc--list-single-notification-test ()
  (ednc--with-temp-server
    (apply #'ednc--notify (ednc--get-test-args))
    (should (string-equal (list-notifications) "  [test: foo]
bar baz
"))))

(ert-deftest ednc--list-multiple-notifications-test ()
  (ednc--with-temp-server
    (apply #'ednc--notify (ednc--get-test-args))
    (apply #'ednc--notify (ednc--get-test-args '(app-name . "tes1")))
    (should (string-equal (list-notifications) "  [tes1: foo]
bar baz
  [test: foo]
bar baz
"))))

;; Test use case 2:
(ert-deftest ednc--stack-no-notifications-test ()
  (ednc--with-temp-server
    (should (string-equal "" (stack-notifications)))))

(ert-deftest ednc--stack-hidden-notification-test ()
  (ednc--with-temp-server
    (apply #'ednc--notify (ednc--get-test-args))
    (should (string-equal "" (stack-notifications '("test"))))))

(ert-deftest ednc--stack-for-single-notification-test ()
  (ednc--with-temp-server
    (apply #'ednc--notify (ednc--get-test-args))
    (should (string-equal (stack-notifications) "  [test: foo]
bar baz
"))))

(ert-deftest ednc--stack-non-stacking-notifications-test ()
  (ednc--with-temp-server
    (apply #'ednc--notify (ednc--get-test-args))
    (apply #'ednc--notify (ednc--get-test-args '(app-name . "tes1")))
    (should (string-equal (stack-notifications) "  [tes1: foo]
bar baz
  [test: foo]
bar baz
"))))

(ert-deftest ednc--stack-stacking-notifications-test ()
  (ednc--with-temp-server
    (apply #'ednc--notify (ednc--get-test-args))
    (apply #'ednc--notify (ednc--get-test-args '(summary . "bar")))
    (should (string-equal (stack-notifications) "  [test: bar]
bar baz
"))))

;; Test logging:
(ert-deftest ednc--log-single-notification-test ()
  (ednc--with-temp-server
    (apply #'ednc--notify (ednc--get-test-args))
    (ednc--update-log-buffer nil (cadr ednc--state))
    (with-current-buffer ednc-log-name
      (should (string-equal (buffer-string) "  [test: foo]
bar baz

")))))

(ert-deftest ednc--log-multiple-notifications-test ()
    (ednc--with-temp-server
      (apply #'ednc--notify (ednc--get-test-args))
      (ednc--update-log-buffer nil (cadr ednc--state))
      (apply #'ednc--notify (ednc--get-test-args '(app-name . "tes1")))
      (ednc--update-log-buffer nil (cadr ednc--state))
      (with-current-buffer ednc-log-name
        (should (string-equal (buffer-string) "  [test: foo]
bar baz

  [tes1: foo]
bar baz

")))))

(ert-deftest ednc--log-closed-notifications-test ()
  (ednc--with-temp-server
    (let* ((id (apply #'ednc--notify (ednc--get-test-args)))
           (notification (cl-find id (cdr ednc--state)
                                  :key #'ednc-notification-id)))
      (ednc--update-log-buffer nil notification)
      (apply #'ednc--notify (ednc--get-test-args '(app-name . "tes1")))
      (ednc--update-log-buffer nil (cadr ednc--state))
      (ednc--close-notification (cadr ednc--state) 3)
      (ednc--update-log-buffer notification nil)
      (with-current-buffer ednc-log-name
        (should (string-equal (buffer-string) "  [test: foo]
bar baz

  [tes1: foo]
bar baz

"))
        (should (equal (get-text-property (point-min) 'face)
                       '(:strike-through t)))))))

(ert-deftest ednc--log-replaced-notifications-test ()
  (ednc--with-temp-server
    (let* ((id (apply #'ednc--notify (ednc--get-test-args)))
           (notification (cl-find id (cdr ednc--state)
                                  :key #'ednc-notification-id)))
      (ednc--update-log-buffer nil notification)
      (apply #'ednc--notify (ednc--get-test-args `(replaces-id . ,id)))
      (ednc--update-log-buffer notification (cadr ednc--state))
      (with-current-buffer ednc-log-name
        (should (string-equal (buffer-string) "  [test: foo]
bar baz

  [test: foo]
bar baz

"))
        (should (equal (get-text-property (point-min) 'face)
                       '(:strike-through t)))))))

;; Test ednc-invoke-action:
(ert-deftest ednc--invoke-default-action-test ()
  (ednc--with-temp-server
    (apply #'ednc--notify (ednc--get-test-args))
    (ednc-invoke-action (cadr ednc--state))))  ; no real test yet

(ert-deftest ednc--invoke-alternative-action-test ()
  (ednc--with-temp-server
    (apply #'ednc--notify (ednc--get-test-args))
    (ednc-invoke-action (cadr ednc--state) "other")))  ; no real test yet

;; Test ednc--close-notification:
(ert-deftest ednc--close-notification-test ()
  (ednc--with-temp-server
    (let ((id (apply #'ednc--notify (ednc--get-test-args))))
      (ednc--close-notification (cadr ednc--state) 3)
      (should-not (cl-find id (cdr ednc--state) :key #'ednc-notification-id)))))

;; Test ednc--format-notification:
(ert-deftest ednc--format-notification-test ()
  (ednc--with-temp-server
    (apply #'ednc--notify (ednc--get-test-args))
    (should (string-equal (ednc-format-notification (cadr ednc--state))
                          "  [test: foo]
bar baz
"))))

;; Test ednc--format-summary:
(ert-deftest ednc--format-summary-test ()
  (ednc--with-temp-server
    (apply #'ednc--notify (ednc--get-test-args))
    (let* ((new (cadr ednc--state))
           (result (ednc--format-summary new)))
      (should (string-equal result (ednc-notification-summary new))))))

;; Test ednc--get-actions-keymap:
(ert-deftest ednc--get-empty-actions-keymap-test ()
  (ednc--with-temp-server
    (apply #'ednc--notify (ednc--get-test-args '(actions)))
    (let* ((new (cadr ednc--state))
           (result (ednc--get-actions-keymap new)))
      (should (eq (car result) 'keymap))
      (should (string-equal (cadr result) "Actions")))))

(ert-deftest ednc--get-actions-keymap-test ()
  (ednc--with-temp-server
    (apply #'ednc--notify (ednc--get-test-args))
    (let* ((new (cadr ednc--state))
           (result (ednc--get-actions-keymap new)))
      (should (eq (car result) 'keymap))
      (dotimes (i 2)
        (let ((entry (cadr result)))
          (should (and (= (car entry) i) (eq (cadr entry) 'menu-item)))
          (should (string-equal (caddr entry)
                                (nth (1+ (* i 2))  ; odd elements
                                     (ednc-notification-actions new))))
          (should (functionp (cadddr entry))))
        (setq result (cdr result)))
      (should (string-equal (cadr result) "Actions")))))

;; Test handler of Notify:
(ert-deftest ednc--handle-notify-test ()
  (ednc--with-temp-server
    (let ((id (apply #'ednc--dbus-talk 'dbus-call-method "Notify"
                     (ednc--get-test-args))))
      (ednc--test-args-match id (ednc--get-test-args)))))

(ert-deftest ednc--handle-notify-with-expiration-time-test ()
  (ednc--with-temp-server
    (let* ((timeout 5)
           (args (ednc--get-test-args `(expire-timeout . ,timeout)))
           (id (apply #'ednc--dbus-talk 'dbus-call-method "Notify" args)))
      (ednc--test-args-match id args)
      (sleep-for (/ (* 2 timeout) 1000.0))
      (should-not (cl-find id (cdr ednc--state) :key #'ednc-notification-id)))))

(ert-deftest ednc--handle-notify-replace-test ()
  (ednc--with-temp-server
    (let* ((id (apply #'ednc--notify (ednc--get-test-args)))
           (args (ednc--get-test-args `(replaces-id . ,id))))
      (apply #'ednc--dbus-talk 'dbus-call-method "Notify" args)
      (ednc--test-args-match id args))))

(ert-deftest ednc--handle-notify-replace-nonexistent-test ()
  (ednc--with-temp-server
    (let* ((id (apply #'ednc--notify (ednc--get-test-args)))
           (args (ednc--get-test-args `(replaces-id . ,(+ 5 id)))))
      (ednc--test-args-match id (ednc--get-test-args))
      (setq id (apply #'ednc--dbus-talk 'dbus-call-method "Notify" args))
      (ednc--test-args-match id (ednc--get-test-args)))))

;; Test handler of CloseNotification:
(ert-deftest ednc--handle-close-notification-test ()
  (ednc--with-temp-server
    (let ((id (apply #'ednc--notify (ednc--get-test-args))))
      (ednc--dbus-talk 'dbus-call-method "CloseNotification" id)
      (should-not (cl-find id (cdr ednc--state) :key #'ednc-notification-id)))))

(ert-deftest ednc--handle-close-previously-closed-notification-test ()
  (ednc--with-temp-server
    (let ((id (apply #'ednc--notify (ednc--get-test-args))))
      (ednc--dbus-talk 'dbus-call-method "CloseNotification" id)
      (should-error (ednc--dbus-talk 'dbus-call-method "CloseNotification"
                                     id)))))

(ert-deftest ednc--handle-close-nonexistent-notification-test ()
  (ednc--with-temp-server
    (let ((unused (1+ (apply #'ednc--notify (ednc--get-test-args)))))
      (should-error (ednc--dbus-talk 'dbus-call-method "CloseNotification"
                                     unused)))))

;; Test informational handlers:
(ert-deftest ednc--handle-get-server-information-test ()
  (ednc--with-temp-server
    (let ((info (ednc--dbus-talk 'dbus-call-method "GetServerInformation")))
      (should (and (listp info) (= (length info) 4)))  ; correct aggregate type
      (dolist (field info)
        (should (stringp field))))))  ; and correct types in aggregate?

(ert-deftest ednc--handle-get-capabilities-test ()
  (ednc--with-temp-server
    (let ((capabilities (ednc--dbus-talk 'dbus-call-method "GetCapabilities")))
      (should (listp capabilities))  ; correct aggregate type?
      (dolist (capability capabilities)
        (should (stringp capability)))  ; correct types in aggregate,
      (dolist (required '("actions" "body"))  ; and minimal feature set?
        (should (member required capabilities))))))

;; Test ednc--get-hint:
(ert-deftest ednc--get-hint-test ()
  (let ((hints '(("foo" (("bar" "baz"))) ("qux" ("quux")))))
    (should (equal '("bar" "baz") (ednc--get-hint hints "foo")))
    (should (string-equal "quux" (ednc--get-hint hints "qux")))))

(ert-deftest ednc--get-and-remove-hint-test ()
  (let ((hints (list (list "foo" '(("bar" "baz"))) (list "qux" '("quux")))))
    (should (equal '("bar" "baz") (ednc--get-hint hints "foo" t)))
    (should-not (ednc--get-hint hints "foo"))  ; older gone, and
    (should (string-equal "quux" (ednc--get-hint hints "qux" t)))
    (should-not (ednc--get-hint hints "qux"))))  ; newest gone?

(ert-deftest ednc--get-or-remove-nonexistent-hint-test ()
  (let ((hints nil))
    (should-not (ednc--get-hint hints "foo" t))  ; neither remove,
    (should-not (ednc--get-hint hints "foo"))))  ; nor get only?

;; Test ednc--path-to-image and ednc--data-to-image:
(ert-deftest ednc--nil-to-image-test ()
  (should-not (ednc--path-to-image nil))
  (should-not (ednc--data-to-image nil)))

(ert-deftest ednc--test-unsupported-paths-to-image ()
  (should-not (ednc--path-to-image "/ne"))  ; no schema, shorter than "file://",
  (should-not (ednc--path-to-image "/nonexistent"))  ; and longer than "file://"
  (should-not (ednc--path-to-image
               "https://www.gnu.org/software/emacs/images/emacs.png")))

(ert-deftest ednc--nonexistent-path-to-image-test ()
  (should-not (ednc--path-to-image "file:///nonexistent")))

(ert-deftest ednc--unsupported-data-to-image-test ()
  (let ((raw (append "abcABCxyzXYZ" nil)))
    (should-not (ednc--data-to-image (list 2 2 6 t 7 3 raw)))  ; bit-depth
    (should-not (ednc--data-to-image (list 2 2 6 t 8 2 raw)))  ; non-RGB(A)
    (should-not (ednc--data-to-image (list 2 2 6 t 8 5 raw)))))

(ert-deftest ednc--data-to-image-test ()
  (let* ((expect "P6\n2 2\n255\nabcABCxyzXYZ")
         (data (append "abcABCxyzXYZ" nil))
         (image (ednc--data-to-image (list 2 2 6 nil 8 3 data))))
    (should (equal (image-property image :data) expect))  ; RGB
    (setq data (append "abc!ABC?xyz?XYZ!" nil)
          image (ednc--data-to-image (list 2 2 8 t 8 4 data)))
    (should (equal (image-property image :data) expect))))  ; RGBA

;; Test ednc--delete-padding:
(ert-deftest ednc--delete-padding-from-empty-list-test ()
  (let ((list (list)))
    (ednc--delete-padding list 3 5)
    (should (null list))))

(ert-deftest ednc--delete-zero-length-padding-from-list-test ()
  (let ((list (list 'foo 'bar 'baz 'qux 'quux 'corge)))
    (ednc--delete-padding list 3 3)
    (should (equal list '(foo bar baz qux quux corge)))))

(ert-deftest ednc--delete-padding-from-short-list-test ()
  (let ((list (list 'foo 'bar 'baz 'qux 'quux)))
    (ednc--delete-padding list 3 5)
    (should (equal list '(foo bar baz)))))

(ert-deftest ednc--delete-alpha-channel-from-short-list-test ()
  (let ((list (list 'foo 'bar 'baz 'qux)))
    (ednc--delete-padding list 3 4)
    (should (equal list '(foo bar baz)))))

(ert-deftest ednc--delete-padding-from-longer-list-test ()
    (let ((list (list 'foo 'bar 'baz 'qux 'quux 'corge)))
      (ednc--delete-padding list 1 3)
      (should (equal list '(foo qux)))))

(ert-deftest ednc--delete-alpha-channel-from-longer-list-test ()
  (let ((list (list 'foo 'bar 'baz 'qux 'quux 'corge 'grault 'garply)))
    (ednc--delete-padding list 3 4)
    (should (equal list '(foo bar baz quux corge grault)))))

;;; test.el ends here
