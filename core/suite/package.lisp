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
  (:import-from #:rove/core/assertion
                #:quit-early)
  (:import-from #:rove/core/marks
                #:*mark-expr*
                #:test-selected-by-marks-p)
  (:export #:all-suites
           #:*shard*
           #:*shard-count*
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
           #:selected-suite-tests
           #:check-shard
           #:test-shard
           #:package-suite
           #:run-suite-tests
           #:*before-test-hooks*
           #:*after-test-hooks*))
(in-package #:rove/core/suite/package)

(deftype string-designator () '(or character string symbol))
(deftype package-designator () '(or package string-designator))

(defvar *package-suites*
  (make-hash-table :test 'eq))

(defvar *before-test-hooks* nil
  "A list of functions of no arguments, each called before every test in every
suite, in addition to per-suite DEFHOOK :before hooks. Intended for global
per-test setup, e.g. quiescing background threads between tests.")

(defvar *after-test-hooks* nil
  "A list of functions of no arguments, each called after every test in every
suite, in addition to per-suite DEFHOOK :after hooks. Each runs in the per-test
UNWIND-PROTECT cleanup, so they execute even when the test errors. Intended for
global per-test cleanup, e.g. quiescing background threads between tests.")

(defvar *shard* nil)
(defvar *shard-count* nil)

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
    (check-type package-name string-designator)
    (let ((package (find-package package-name)))
      (unless package
        (error "No package '~A' found" package-name))
      (find-suite package))))

(defun package-suite (package)
  (check-type package package-designator)
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

(defun check-shard (shard shards)
  (when (or shard shards)
    (unless (and (integerp shard) (integerp shards) (<= 0 shard) (< shard shards))
      (error "Invalid shard: :shard ~S :shards ~S (expected integers with 0 <= shard < shards)"
             shard shards))))

(defun test-shard (name shards)
  "FNV-1a of the qualified test name modulo SHARDS, so the split doesn't depend on suite or definition order."
  (let ((hash 2166136261))
    (flet ((mix (string)
             (loop for char across string
                   do (setf hash (logand (* (logxor hash (char-code char)) 16777619)
                                         #xFFFFFFFF)))))
      (let ((package (symbol-package name)))
        (when package
          (mix (package-name package))))
      (mix "::")
      (mix (symbol-name name)))
    (mod hash shards)))

(defun selected-suite-tests (suite)
  (let ((tests (suite-tests suite)))
    (when *mark-expr*
      (setf tests (remove-if-not #'test-selected-by-marks-p tests)))
    (when *shard-count*
      (check-shard *shard* *shard-count*)
      (setf tests (remove-if-not (lambda (test)
                                   (= (test-shard test *shard-count*) *shard*))
                                 tests)))
    tests))

(defgeneric run-suite-tests (suite)
  (:method (suite)
    (run-suite-tests (package-suite suite))))

(defmethod run-suite-tests ((suite suite))
  (let* ((suite-name (suite-name suite))
         (*package* (suite-package suite))
         (tests (selected-suite-tests suite)))
    (when (toplevel-stats-p *stats*)
      (initialize *stats*))
    (unless (and (null tests) (or *mark-expr* *shard-count*))
      (suite-begin *stats* suite-name)
      (handler-case
          (with-context (context :name suite-name)
            (unwind-protect
                 (progn
                   (when (suite-setup suite)
                     (funcall (suite-setup suite)))
                   (dolist (test tests)
                     (unwind-protect
                         (progn
                           (mapc #'funcall (reverse *before-test-hooks*))
                           (mapc #'run-hook (reverse (suite-before-hooks suite)))
                           (funcall (get-test test)))
                       (mapc #'run-hook (reverse (suite-after-hooks suite)))
                       (mapc #'funcall (reverse *after-test-hooks*)))))
              (when (suite-teardown suite)
                (funcall (suite-teardown suite)))))
        (quit-early ()))
      (suite-finish *stats* suite-name))
    (when (toplevel-stats-p *stats*)
      (summarize *stats*))
    (values (passedp *stats*)
            (stats-results *stats*))))
