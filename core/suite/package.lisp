(in-package #:cl-user)
(defpackage #:rove/core/suite/package
  (:use #:cl)
  (:import-from #:rove/core/suite/file
                #:resolve-file
                #:file-package
                #:system-packages)
  (:export #:all-suites
           #:system-suites
           #:*execute-assertions*
           #:wrap-if-toplevel
           #:get-test
           #:set-test
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

(defun system-suites (system)
  (mapcar (lambda (package)
            (gethash package *package-suites*))
          (system-packages system)))

(defstruct suite
  name
  setup
  teardown
  before-hooks
  after-hooks
  tests)

(defvar *execute-assertions* t)

(defun make-new-suite (package)
  (let ((pathname (resolve-file (or *load-pathname* *compile-file-pathname*))))
    (when (and pathname
               (not (file-package pathname nil)))
      (setf (file-package pathname) package)))
  (make-suite :name (package-name package)))

(defun package-suite (package)
  (or (gethash package *package-suites*)
      (setf (gethash package *package-suites*)
            (make-new-suite package))))

(defun get-test (name)
  (check-type name symbol)
  (get name 'test))

(defun set-test (name test-fn)
  (pushnew name (suite-tests (package-suite *package*))
           :test 'eq)
  (setf (get name 'test) test-fn)
  name)

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
                  (funcall (get-test test)))
             (mapc #'funcall (reverse (suite-after-hooks suite))))))
    (when (suite-teardown suite)
      (funcall (suite-teardown suite)))))
