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

(require 'notifications)

;; Load the use cases documented in README.org:
(require 'ob-tangle)
(load-file (car (org-babel-tangle-file "README.org")))

;; Helpers for testing:
(defmacro ednc--with-temp-server (&rest body)
  (declare (indent defun))
  `(let ((ednc-log-name "*ednc-test-log*")
         (ednc-notification-amendment-functions)
         (ednc-notification-presentation-functions)
         (ednc--state (list 0)))
     (skip-unless (not ednc-mode))  ; conflicting servers?
     (ednc--start-server)
     ,@body
     (ednc--test-state-consistency)
     (ednc--stop-server)
     (if (get-buffer ednc-log-name)
         (kill-buffer ednc-log-name))))

(defconst ednc--default-test-args
  '(:title "foo" :body "bar baz" :app-name "test" :replaces-id 0 :app-icon nil
           :actions ("default" "qux" "other" "quux") :timeout 0))

(defun ednc--test-arg-matches (notification slot plist prop)
  (should (equal (cl-struct-slot-value 'ednc-notification slot notification)
                 (plist-get plist prop))))

(defun ednc--test-args-match (id args)
  (let ((found (cl-find id (cdr ednc--state) :key #'ednc-notification-id)))
    (ednc--test-arg-matches found 'summary args :title)
    (if (plist-get args :body)
        (ednc--test-arg-matches found 'body args :body))
    (ednc--test-arg-matches found 'app-name args :app-name)
    (if (zerop (plist-get args :replaces-id))
        (should-not (zerop (ednc-notification-id found)))
      (ednc--test-arg-matches found 'id args :replaces-id))
    ;(ednc--test-arg-matches found 'app-icon args :app-icon)
    (ednc--test-arg-matches found 'actions args :actions)
    (if (zerop (plist-get args :timeout))
        (should-not (ednc-notification-timer found))
      (should (timerp (ednc-notification-timer found))))
    (should (ednc-notification-client found))
    (should (ednc-notification-parent found))))

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

(ert-deftest ednc--test-match-of-matching-arguments-test ()
  (ednc--with-temp-server
    (let* ((args (nconc (list :body "baz bar") ednc--default-test-args))
           (id (apply #'notifications-notify args))
           (test (make-ert-test
                  :body (lambda () (ednc--test-args-match id args)))))
      (should (ert-test-passed-p (ert-run-test test))))))

(ert-deftest ednc--test-match-of-mismatching-arguments-test ()
  (ednc--with-temp-server
    (let* ((args (nconc (list :body "baz bar") ednc--default-test-args))
           (id (apply #'notifications-notify ednc--default-test-args))
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

;; Test use case list-notifcations:
(ert-deftest ednc--list-no-notifications-test ()
  (ednc--with-temp-server
    (should (string-equal "" (list-notifications)))))

(ert-deftest ednc--list-single-notification-test ()
  (ednc--with-temp-server
    (apply #'notifications-notify ednc--default-test-args)
    (should (string-equal (list-notifications) " [test: foo]
bar baz
"))))

(ert-deftest ednc--list-multiple-notifications-test ()
  (ednc--with-temp-server
    (apply #'notifications-notify ednc--default-test-args)
    (apply #'notifications-notify :app-name "tes1" ednc--default-test-args)
    (should (string-equal (list-notifications) " [tes1: foo]
bar baz
 [test: foo]
bar baz
"))))

;; Test use case stack-notifications:
(ert-deftest ednc--stack-no-notifications-test ()
  (ednc--with-temp-server
    (should (string-equal "" (stack-notifications)))))

(ert-deftest ednc--stack-hidden-notification-test ()
  (ednc--with-temp-server
    (apply #'notifications-notify ednc--default-test-args)
    (should (string-equal "" (stack-notifications '("test"))))))

(ert-deftest ednc--stack-for-single-notification-test ()
  (ednc--with-temp-server
    (apply #'notifications-notify ednc--default-test-args)
    (should (string-equal (stack-notifications) " [test: foo]
bar baz
"))))

(ert-deftest ednc--stack-non-stacking-notifications-test ()
  (ednc--with-temp-server
    (apply #'notifications-notify ednc--default-test-args)
    (apply #'notifications-notify :app-name "tes1" ednc--default-test-args)
    (should (string-equal (stack-notifications) " [tes1: foo]
bar baz
 [test: foo]
bar baz
"))))

(ert-deftest ednc--stack-stacking-notifications-test ()
  (ednc--with-temp-server
    (apply #'notifications-notify ednc--default-test-args)
    (apply #'notifications-notify :title "bar" ednc--default-test-args)
    (should (string-equal (stack-notifications) " [test: bar]
bar baz
"))))

;; Test use case show-notification-in-buffer:
(ert-deftest ednc--show-notification-in-buffer-test ()
  (ednc--with-temp-server
    (apply #'notifications-notify ednc--default-test-args)
    (with-temp-buffer
      (rename-buffer "Notification 1")
      (show-notification-in-buffer nil (cadr ednc--state))
      (should (string-equal (buffer-string) " [test: foo]
bar baz
")))))

;; Test logging:
(ert-deftest ednc--log-single-notification-test ()
  (ednc--with-temp-server
    (apply #'notifications-notify ednc--default-test-args)
    (ednc--update-log-buffer nil (cadr ednc--state))
    (with-current-buffer ednc-log-name
      (should (string-equal (buffer-string) " [test: foo]
bar baz

")))))

(ert-deftest ednc--log-multiple-notifications-test ()
    (ednc--with-temp-server
      (apply #'notifications-notify ednc--default-test-args)
      (ednc--update-log-buffer nil (cadr ednc--state))
      (apply #'notifications-notify :app-name "tes1" ednc--default-test-args)
      (ednc--update-log-buffer nil (cadr ednc--state))
      (with-current-buffer ednc-log-name
        (should (string-equal (buffer-string) " [test: foo]
bar baz

 [tes1: foo]
bar baz

")))))

(ert-deftest ednc--log-closed-notifications-test ()
  (ednc--with-temp-server
    (let* ((id (apply #'notifications-notify ednc--default-test-args))
           (notification (cl-find id (cdr ednc--state)
                                  :key #'ednc-notification-id)))
      (ednc--update-log-buffer nil notification)
      (apply #'notifications-notify :app-name "tes1" ednc--default-test-args)
      (ednc--update-log-buffer nil (cadr ednc--state))
      (notifications-close-notification (ednc-notification-id
                                         (cadr ednc--state)))
      (ednc--update-log-buffer notification nil)
      (with-current-buffer ednc-log-name
        (should (string-equal (buffer-string) " [test: foo]
bar baz

 [tes1: foo]
bar baz

"))
        (should (equal (get-text-property (point-min) 'face)
                       '(:strike-through t)))))))

(ert-deftest ednc--log-replaced-notifications-test ()
  (ednc--with-temp-server
    (let* ((id (apply #'notifications-notify ednc--default-test-args))
           (notification (cl-find id (cdr ednc--state)
                                  :key #'ednc-notification-id)))
      (ednc--update-log-buffer nil notification)
      (apply #'notifications-notify :replaces-id id ednc--default-test-args)
      (ednc--update-log-buffer notification (cadr ednc--state))
      (with-current-buffer ednc-log-name
        (should (string-equal (buffer-string) " [test: foo]
bar baz

 [test: foo]
bar baz

"))
        (should (equal (get-text-property (point-min) 'face)
                       '(:strike-through t)))))))

;; Test ednc-invoke-action:
(ert-deftest ednc--invoke-default-action-test ()
  (ednc--with-temp-server
    (apply #'notifications-notify
           :on-action (lambda (id key) (should (string-equal key "default")))
           ednc--default-test-args)
    (ednc-invoke-action (cadr ednc--state))))

(ert-deftest ednc--invoke-alternative-action-test ()
  (ednc--with-temp-server
    (apply #'notifications-notify
           :on-action (lambda (id key) (should (string-equal key "other")))
           ednc--default-test-args)
    (ednc-invoke-action (cadr ednc--state) "other")))

;; Test ednc--close-notification:
(ert-deftest ednc--close-notification-test ()
  (ednc--with-temp-server
    (let ((id (apply #'notifications-notify ednc--default-test-args)))
      (ednc--close-notification (cadr ednc--state) 3)
      (should-not (cl-find id (cdr ednc--state) :key #'ednc-notification-id)))))

;; Test ednc--format-notification:
(ert-deftest ednc--format-notification-test ()
  (ednc--with-temp-server
    (apply #'notifications-notify ednc--default-test-args)
    (should (string-equal (ednc-format-notification (cadr ednc--state))
                          " [test: foo]
bar baz
"))))

;; Test ednc--format-summary:
(ert-deftest ednc--format-summary-test ()
  (ednc--with-temp-server
    (apply #'notifications-notify ednc--default-test-args)
    (let* ((new (cadr ednc--state))
           (result (ednc--format-summary new)))
      (should (string-equal result (ednc-notification-summary new))))))

;; Test ednc--get-actions-keymap:
(ert-deftest ednc--get-empty-actions-keymap-test ()
  (ednc--with-temp-server
    (apply #'notifications-notify :actions nil ednc--default-test-args)
    (let* ((new (cadr ednc--state))
           (result (ednc--get-actions-keymap new)))
      (should (eq (car result) 'keymap))
      (should (string-equal (cadr result) "Actions")))))

(ert-deftest ednc--get-actions-keymap-test ()
  (ednc--with-temp-server
    (apply #'notifications-notify ednc--default-test-args)
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
    (let ((id (apply #'notifications-notify ednc--default-test-args)))
      (ednc--test-args-match id ednc--default-test-args))))

(ert-deftest ednc--handle-notify-with-expiration-time-test ()
  (ednc--with-temp-server
    (let* ((timeout 5)
           (args (nconc (list :timeout timeout) ednc--default-test-args))
           (id (apply #'notifications-notify args)))
      (ednc--test-args-match id args)
      (sleep-for (/ (* 2 timeout) 1000.0))
      (should-not (cl-find id (cdr ednc--state) :key #'ednc-notification-id)))))

(ert-deftest ednc--handle-notify-replace-test ()
  (ednc--with-temp-server
    (let* ((id (apply #'notifications-notify ednc--default-test-args))
           (args (nconc (list :replaces-id id) ednc--default-test-args)))
      (apply #'notifications-notify args)
      (ednc--test-args-match id args))))

(ert-deftest ednc--handle-notify-replace-nonexistent-test ()
  (ednc--with-temp-server
    (let* ((id (apply #'notifications-notify ednc--default-test-args))
           (args (nconc (list :replaces-id (+ 5 id)) ednc--default-test-args)))
      (ednc--test-args-match id ednc--default-test-args)
      (setq id (apply #'notifications-notify args))
      (ednc--test-args-match id ednc--default-test-args))))

;; Test handler of CloseNotification:
(ert-deftest ednc--handle-close-notification-test ()
  (ednc--with-temp-server
    (let ((id (apply #'notifications-notify ednc--default-test-args)))
      (notifications-close-notification id)
      (should-not (cl-find id (cdr ednc--state) :key #'ednc-notification-id)))))

(ert-deftest ednc--handle-close-previously-closed-notification-test ()
  (ednc--with-temp-server
    (let ((id (apply #'notifications-notify ednc--default-test-args)))
      (notifications-close-notification id)
      (should-error (notifications-close-notification id)))))

(ert-deftest ednc--handle-close-nonexistent-notification-test ()
  (ednc--with-temp-server
    (let ((unused (1+ (apply #'notifications-notify ednc--default-test-args))))
      (should-error (notifications-close-notification unused)))))

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
