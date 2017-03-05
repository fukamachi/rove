(in-package #:cl-user)
(defpackage #:rove/core/test
  (:use #:cl
        #:rove/core/conditions)
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

(defun run-test (test)
  (check-type test symbol)
  (assert (fboundp test))
  (let ((passed-tests '())
        (failed-tests '())
        (test-name (let ((*print-case* :downcase))
                     (princ-to-string test))))

    (signal 'test-begin :name test-name)

    (unwind-protect
         (handler-bind ((passed
                          (lambda (c)
                            (push c passed-tests)))
                        (failed
                          (lambda (c)
                            (push c failed-tests))))
           (funcall test))

      (let ((passedp (null failed-tests)))
        (signal (if passedp
                    'test-passed
                    'test-failed)
                :name test-name
                :passed (nreverse passed-tests)
                :failed (nreverse failed-tests))

        passedp))))

(defun run-package-tests (package)
  (check-type package package)
  (let ((test-name (string-downcase (package-name package)))
        (tests (package-tests package))
        (passedp t))
    (signal 'package-tests-begin
            :name test-name
            :count (length tests))
    (unwind-protect
         (dolist (test tests)
           (handler-bind ((test-failed
                            (lambda (c)
                              (declare (ignore c))
                              (setf passedp nil))))
             (run-test test)))
      (signal (if passedp
                  'package-tests-passed
                  'package-tests-failed)
              :name test-name))
    passedp))
