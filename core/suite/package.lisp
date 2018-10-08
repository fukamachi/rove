(in-package #:cl-user)
(defpackage #:rove/core/suite/package
  (:use #:cl)
  (:export #:all-suites
           #:*execute-assertions*
           #:wrap-if-toplevel
           #:suite-name
           #:suite-setup
           #:suite-teardown
           #:suite-before-hooks
           #:suite-after-hooks
           #:suite-tests
           #:package-suite
           #:run-suite))
(in-package #:rove/core/suite/package)

(defvar *package-suites*
  (make-hash-table :test 'eq))

(defun all-suites ()
  (loop for suite being the hash-value of *package-suites*
        collect suite))

(defstruct suite
  name
  setup
  teardown
  before-hooks
  after-hooks
  tests)

(defvar *execute-assertions* t)

(defun package-suite (package)
  (or (gethash package *package-suites*)
      (setf (gethash package *package-suites*)
            (make-suite :name (package-name package)))))

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

(defun run-suite (suite)
  (unwind-protect
       (progn
         (when (suite-setup suite)
           (funcall (suite-setup suite)))
         (dolist (test (reverse (suite-tests suite)))
           (unwind-protect
                (progn
                  (mapc #'funcall (reverse (suite-before-hooks suite)))
                  (funcall test))
             (mapc #'funcall (reverse (suite-after-hooks suite))))))
    (when (suite-teardown suite)
      (funcall (suite-teardown suite)))))
