(in-package #:cl-user)
(defpackage #:rove/core/suite/package
  (:use #:cl)
  (:export #:*execute-assertions*
           #:wrap-if-toplevel
           #:suite-setup-functions
           #:suite-teardown-functions
           #:suite-before-hooks
           #:suite-after-hooks
           #:suite-tests
           #:package-suite))
(in-package #:rove/core/suite/package)

(defvar *package-suites*
  (make-hash-table :test 'eq))

(defstruct suite
  setup-functions
  teardown-functions
  before-hooks
  after-hooks
  tests)

(defvar *execute-assertions* t)

(defun package-suite (package)
  (or (gethash package *package-suites*)
      (setf (gethash package *package-suites*)
            (make-suite))))

(defmacro wrap-if-toplevel (&body body)
  (let ((main (gensym "MAIN")))
    `(flet ((,main () ,@body))
       (if *execute-assertions*
           (,main)
           (progn
             (pushnew (lambda ()
                        (let ((*execute-assertions* t))
                          (,main)))
                      (suite-tests (package-suite *package*))
                      :test 'eq)
             (values))))))
