(in-package #:cl-user)
(defpackage #:rove/core/suite/package
  (:use #:cl)
  (:import-from #:rove/core/suite/file
                #:resolve-file
                #:file-package
                #:system-packages)
  (:import-from #:rove/core/stats
                #:*stats*
                #:stats-context
                #:stats-results
                #:with-context
                #:suite-begin
                #:suite-finish
                #:passedp
                #:initialize
                #:summarize
                #:toplevel-stats-p)
  (:export #:all-suites
           #:find-suite
           #:system-suites
           #:get-test
           #:set-test
           #:remove-test
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

(defclass suite ()
  ((name :type string
         :initarg :name
         :initform (error ":name is required")
         :accessor suite-name)
   (package :type package
            :initarg :package
            :initform (error ":package is required")
            :accessor suite-package)
   (setup :type (or function null)
          :initarg :setup
          :initform nil
          :accessor suite-setup)
   (teardown :type (or function null)
             :initarg :teardown
             :initform nil
             :accessor suite-teardown)
   (before-hooks :type list
                 :initarg :before-hooks
                 :initform '()
                 :accessor suite-before-hooks)
   (after-hooks :type list
                :initarg :after-hooks
                :initform '()
                :accessor suite-after-hooks)
   (%tests :initform '())))

(defun suite-tests (suite)
  (reverse (remove-if #'null (slot-value suite '%tests) :key #'get-test)))

(defun (setf suite-tests) (value suite)
  (setf (slot-value suite '%tests) value))

(defun make-new-suite (package)
  (let ((pathname (resolve-file (or *load-pathname* *compile-file-pathname*))))
    (when (and pathname
               (not (file-package pathname nil)))
      (setf (file-package pathname) package)))
  (make-instance 'suite
                 :name (string-downcase (package-name package))
                 :package package))

(defgeneric find-suite (package)
  (:method ((package package))
    (values (gethash package *package-suites*)))
  (:method (package-name)
    (let ((package (find-package package-name)))
      (unless package
        (error "No package '~A' found" package-name))
      (find-suite package))))

(defun package-suite (package)
  (or (find-suite package)
      (let ((package (find-package package)))
        (setf (gethash package *package-suites*)
              (make-new-suite package)))))

(defun get-test (name)
  (check-type name symbol)
  (get name 'test))

(defun set-test (name test-fn)
  (check-type name symbol)
  (pushnew name (slot-value (package-suite *package*) '%tests)
           :test 'eq)
  (setf (get name 'test) test-fn)
  name)

(defun remove-test (name)
  (remprop name 'test)
  (values))

(defun run-hook (hook)
  (destructuring-bind (name . fn)
      hook
    (declare (ignore name))
    (funcall fn)))

(defun run-suite (suite)
  (let* ((suite (typecase suite
                  (suite suite)
                  (otherwise (package-suite suite))))
         (suite-name (suite-name suite))
         (*package* (suite-package suite)))
    (when (toplevel-stats-p *stats*)
      (initialize *stats*))
    (suite-begin *stats* suite-name)
    (with-context (context :name suite-name)
      (unwind-protect
          (progn
            (when (suite-setup suite)
              (funcall (suite-setup suite)))
            (dolist (test (suite-tests suite))
              (unwind-protect
                  (progn
                    (mapc #'run-hook (reverse (suite-before-hooks suite)))
                    (funcall (get-test test)))
                (mapc #'run-hook (reverse (suite-after-hooks suite))))))
        (when (suite-teardown suite)
          (funcall (suite-teardown suite)))))
    (suite-finish *stats* suite-name)
    (when (toplevel-stats-p *stats*)
      (summarize *stats*))
    (values (passedp *stats*)
            (stats-results *stats*))))
