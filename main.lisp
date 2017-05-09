(in-package #:cl-user)
(defpackage #:rove/main
  (:nicknames #:rove)
  (:use #:cl)
  (:import-from #:rove/core/assertion
                #:*debug-on-error*
                #:ok
                #:ng
                #:signals
                #:outputs
                #:expands
                #:pass
                #:fail
                #:skip)
  (:import-from #:rove/core/test
                #:deftest
                #:testing)
  (:import-from #:rove/core/suite
                #:run-system-tests)
  (:import-from #:rove/core/stats
                #:*stats*
                #:plan)
  (:import-from #:rove/core/result
                #:form-description)
  (:import-from #:rove/reporter
                #:with-reporter
                #:diag
                #:*report-stream*)
  (:import-from #:rove/misc/color
                #:*enable-colors*)
  (:import-from #:rove/reporter/spec
                #:spec-reporter)
  (:import-from #:uiop)
  (:export #:*debug-on-error*
           #:ok
           #:ng
           #:signals
           #:outputs
           #:expands
           #:pass
           #:fail
           #:skip
           #:deftest
           #:testing
           #:run
           #:*default-env*

           #:form-description

           #:*stats*
           #:plan

           #:diag
           #:with-reporter
           #:*report-stream*
           #:*enable-colors*))
(in-package #:rove/main)

(defvar *default-env* '())

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

(defun run (target &key (style :spec) (env *default-env*))
  (with-local-envs env
    (with-reporter style
      (run-system-tests target))))
