(in-package #:cl-user)
(defpackage #:rove/core/test
  (:use #:cl
        #:rove/core/stats)
  (:export #:deftest
           #:package-tests
           #:clear-package-tests
           #:run-test
           #:run-package-tests))
(in-package #:rove/core/test)

(defvar *package-suites*
  (make-hash-table :test 'eq))

(defmacro deftest (name &body body)
  `(progn
     (pushnew ',name (gethash *package* *package-suites*)
              :test 'eq)
     (defun ,name ()
       ,@body)))

(defun package-tests (package)
  (reverse (gethash package *package-suites*)))

(defun clear-package-tests (package)
  (check-type package package)
  (remhash package *package-suites*))

(defun run-test (test-symbol)
  (check-type test-symbol symbol)
  (assert (fboundp test-symbol))

  (let ((test-name (let ((*print-case* :downcase))
                     (princ-to-string test-symbol))))
    (test-begin *stats* test-name)
    (unwind-protect
         (funcall test-symbol)
      (test-finish *stats* test-name))))

(defun run-package-tests (package)
  (check-type package package)
  (let ((test-name (string-downcase (package-name package)))
        (tests (package-tests package)))
    (test-begin *stats* test-name (length tests))
    (unwind-protect (dolist (test tests)
                      (run-test test))
      (test-finish *stats* test-name))))
