(uiop:define-package #:rove
  (:nicknames #:rove/main)
  (:use #:cl)
  (:use-reexport #:rove/core/assertion)
  (:use-reexport #:rove/core/test)
  (:use-reexport #:rove/core/suite)
  (:use-reexport #:rove/core/result)
  (:use-reexport #:rove/reporter)
  (:import-from #:rove/core/suite
                #:run-system-tests)
  (:import-from #:rove/core/stats
                #:plan)
  (:import-from #:rove/misc/color
                #:*enable-colors*)
  (:import-from #:rove/reporter/spec
                #:spec-reporter)
  (:import-from #:rove/reporter/dot)
  (:import-from #:uiop)
  (:import-from #:cl-ppcre)
  (:export #:run
           #:run*
           #:run-test
           #:with-local-envs
           #:*default-reporter*
           #:*default-env*

           #:plan
           #:*enable-colors*))
(in-package #:rove/main)

(defvar *default-env* '())
(defvar *default-reporter*
  (or (let ((default-reporter-symbol (intern (string :*rove-default-reporter*) :cl-user)))
        (and (boundp default-reporter-symbol)
             (symbol-value default-reporter-symbol)))
      :spec))

(defmacro with-local-envs (env &body body)
  (let ((before-env (gensym "BEFORE-ENV"))
        (k (gensym "K"))
        (v (gensym "V")))
    `(let ((,before-env
             (loop for (,k . ,v) in ,env
                   collect (cons ,k (or (uiop:getenv ,k) ""))
                   do (setf (uiop:getenv ,k) ,v))))
       (unwind-protect (progn ,@body)
         (loop for (,k . ,v) in ,before-env
               do (setf (uiop:getenv ,k) ,v))))))

(defgeneric run-test (test-name &key style)
  (:documentation "Run a single test function."))

(defgeneric run (target &key style env)
  (:documentation "Run a test package."))

(defmethod run-test (test-name &key (style :spec))
  (let ((test (get-test test-name)))
    (unless test
      (error "No test found for ~S" test-name))
    (with-reporter style
      (testing nil
               (funcall test)))))

(defmethod run (target &key (style *default-reporter*) (env *default-env*))
  (with-local-envs env
    (with-reporter style
      (run-system-tests target))))

(defun compile-wild-card (pattern)
  (check-type pattern string)
  (let* ((regex-string
           (format nil "^~{~A~}$"
                   (mapcar (lambda (part)
                             (if (string= "*" part)
                                 ".*"
                                 (ppcre:quote-meta-chars part)))
                           (ppcre:split "(\\*)" pattern :with-registers-p t))))
         (re (ppcre:create-scanner regex-string)))
    (lambda (value)
      (check-type value string)
      (and (ppcre:scan re value)
           t))))

(defun run* (target-pattern &rest args &key style env)
  (declare (ignore style env))
  (let ((target-pattern (etypecase target-pattern
                          (string target-pattern)
                          (symbol (let ((*print-case* :downcase))
                                    (princ-to-string target-pattern))))))
    (let* ((pattern-main-part (first (ppcre:split "\\*" target-pattern :limit 2)))
           (pattern-/-pos (position #\/ pattern-main-part :from-end t)))
      (or (asdf:find-system pattern-main-part nil)
          (asdf:find-system (subseq target-pattern 0 pattern-/-pos) nil)))
    (let* ((matcher (compile-wild-card target-pattern))
           (system-names
             (remove-if-not matcher (asdf:registered-systems))))
      (when system-names
        (apply #'run system-names args)))))

;; Enable the default reporter
(use-reporter *default-reporter*)
