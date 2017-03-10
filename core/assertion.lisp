(in-package #:cl-user)
(defpackage #:rove/core/assertion
  (:use #:cl
        #:rove/core/stats
        #:rove/core/result)
  (:export #:ok
           #:ng
           #:signals
           #:outputs
           #:pass
           #:fail
           #:skip))
(in-package #:rove/core/assertion)

(defun form-steps (form)
  (if (consp form)
      (let ((steps '()))
        (do ((step-form form (macroexpand-1 step-form)))
            ((not (macro-function (first step-form)))
             (cons step-form steps))
          (push step-form steps)))
      (list form)))

(defmacro ok (form &optional desc)
  (let ((values (gensym "VALUES"))
        (result (gensym "RESULT"))
        (reason (gensym "REASON"))
        (e (gensym "E")))
    (let* ((steps (form-steps form))
           (expanded-form (first steps)))
      `(let* ((,reason nil)
              (,values ,@(and (consp expanded-form)
                              `((handler-case (list ,@(rest expanded-form))
                                   (error (,e)
                                     (setf ,reason ,e)
                                     nil)))))
              (,result
                (if ,reason
                    nil
                    (handler-case ,(if (consp expanded-form)
                                       `(apply ',(first expanded-form) ,values)
                                       expanded-form)
                      (error (,e)
                        (setf ,reason ,e)
                        nil)))))
         (record *stats*
                 (make-instance (if ,result
                                    'passed-assertion
                                    'failed-assertion)
                                :form ',form
                                :steps ',(nreverse steps)
                                :args ',(if (consp expanded-form)
                                            (rest expanded-form)
                                            nil)
                                :values ,values
                                :reason ,reason
                                :desc ,desc))
         ,result))))

(defmacro ng (form &optional desc)
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
         (record *stats*
                 (make-instance (cond
                                  (,reason 'failed-assertion)
                                  (,result 'failed-assertion)
                                  (t 'passed-assertion))
                                :form ',form
                                :steps ',(nreverse steps)
                                :args ',(rest expanded-form)
                                :values ,values
                                :reason ,reason
                                :desc ,desc))
         ,result))))

(defmacro signal-of (form)
  `(handler-case (progn ,form nil)
     (condition (c) c)))

(defmacro signals (form &optional (condition ''error))
  `(typep (signal-of ,form) ,condition))

(defmacro output-of (form &optional (stream '*standard-output*))
  `(with-output-to-string (,stream)
     ,form))

(defmacro outputs (form content &optional (stream '*standard-output*))
  `(equal (output-of ,form ,stream) ,content))

(defun pass (desc)
  (record *stats*
          (make-instance 'passed-assertion
                         :form t
                         :desc desc))
  t)

(defun fail (desc)
  (record *stats*
          (make-instance 'failed-assertion
                         :form t
                         :desc desc))
  nil)

(defun skip (desc)
  (record *stats*
          (make-instance 'pending-assertion
                         :form t
                         :desc desc))
  t)
