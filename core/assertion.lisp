(in-package #:cl-user)
(defpackage #:rove/core/assertion
  (:use #:cl
        #:rove/core/conditions)
  (:export #:ok
           #:ng
           #:signals
           #:outputs
           #:pass
           #:fail))
(in-package #:rove/core/assertion)

(defun form-steps (form)
  (let ((steps '()))
    (do ((step-form form (macroexpand-1 step-form)))
        ((not (macro-function (first step-form)))
         (cons step-form steps))
      (push step-form steps))))

(defmacro ok (form &optional desc)
  (check-type form cons)
  (let* ((steps (form-steps form))
         (expanded-form (first steps)))
    (let ((values (gensym "VALUES"))
          (result (gensym "RESULT"))
          (reason (gensym "REASON"))
          (e (gensym "E")))
      `(let* ((,reason nil)
              (,values (list ,@(rest expanded-form)))
              (,result
                (handler-case (apply ',(first expanded-form) ,values)
                  (error (,e)
                    (setf ,reason ,e)
                    nil))))
         (signal (if ,result
                     'passed
                     'failed)
                 :form ',form
                 :steps ',(nreverse steps)
                 :args ',(rest expanded-form)
                 :values ,values
                 :reason ,reason
                 :desc ,desc)
         ,result))))

(defmacro ng (form &optional desc)
  `(handler-case (ok ,form)
     (passed (e)
       (if (assertion-reason e)
           (signal e)
           (signal 'failed
                   :form `(not ,(assertion-form e))
                   :steps (assertion-steps e)
                   :args (assertion-args e)
                   :values (assertion-values e)
                   :desc ,desc)))
     (failed (e)
       (if (assertion-reason e)
           (signal e)
           (signal 'passed
                   :form `(not ,(assertion-form e))
                   :steps (assertion-steps e)
                   :args (assertion-args e)
                   :values (assertion-values e)
                   :desc ,desc)))))

(defmacro signal-of (form)
  `(handler-case (progn ,form nil)
     (condition (c) c)))

(defmacro signals (form &optional (condition 'error))
  `(typep (signal-of ,form) ,condition))

(defmacro output-of (form &optional (stream '*standard-output*))
  `(with-output-to-string (,stream)
     ,form))

(defmacro outputs (form content &optional (stream '*standard-output*))
  `(equal (output-of ,form ,stream) ,content))

(defun pass (desc)
  (signal 'passed
          :form t
          :desc desc)
  t)

(defun fail (desc)
  (signal 'failed
          :form t
          :desc desc)
  nil)
