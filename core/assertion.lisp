(in-package #:cl-user)
(defpackage #:rove/core/assertion
  (:use #:cl)
  (:import-from #:rove/core/conditions
                #:passed
                #:failed)
  (:export #:is
           #:ok
           #:pass
           #:fail))
(in-package #:rove/core/assertion)

(defun form-steps (form)
  (let ((steps '()))
    (do ((step-form form (macroexpand-1 step-form)))
        ((not (macro-function (first step-form)))
         (cons step-form steps))
      (push step-form steps))))

(defmacro is (form &optional desc)
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

(defmacro ok (form &optional desc)
  (let ((result (gensym "RESULT")))
    `(let ((,result ,form))
       (signal (if ,result
                   'passed
                   'failed)
               :form ',form
               :desc ,desc)
       ,result)))

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
