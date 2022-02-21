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
  (:export #:run
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

;; Enable the default reporter
(use-reporter *default-reporter*)
