(in-package #:cl-user)
(defpackage #:rove/core/suite/package
  (:use #:cl)
  (:export #:*package-suites*
           #:*execute-assertions*
           #:wrap-if-toplevel))
(in-package #:rove/core/suite/package)

(defvar *package-suites*
  (make-hash-table :test 'eq))

(defvar *execute-assertions* t)

(defmacro wrap-if-toplevel (&body body)
  (let ((main (gensym "MAIN")))
    `(flet ((,main () ,@body))
       (if *execute-assertions*
           (,main)
           (progn
             (pushnew (lambda ()
                        (let ((*execute-assertions* t))
                          (,main)))
                      (gethash *package* *package-suites*)
                      :test 'eq)
             (values))))))
