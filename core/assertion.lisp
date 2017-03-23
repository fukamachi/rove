(in-package #:cl-user)
(defpackage #:rove/core/assertion
  (:use #:cl
        #:rove/core/stats
        #:rove/core/result)
  (:import-from #:dissect
                #:stack)
  (:export #:*debug-on-error*
           #:ok
           #:ng
           #:signals
           #:outputs
           #:pass
           #:fail
           #:skip))
(in-package #:rove/core/assertion)

(defvar *debug-on-error* nil)

(defun form-steps (form)
  (if (consp form)
      (let ((steps '()))
        (do ((step-form form (macroexpand-1 step-form)))
            ((or (eq (symbol-package (first step-form)) (find-package :cl))
                 (special-operator-p (first step-form))
                 (not (macro-function (first step-form))))
             (cons step-form steps))
          (push step-form steps)))
      (list form)))

(defmacro %okng (form desc class-fn)
  (let ((values (gensym "VALUES"))
        (result (gensym "RESULT"))
        (reason (gensym "REASON"))
        (stacks (gensym "STACKS"))
        (e (gensym "E")))
    (let* ((steps (form-steps form))
           (expanded-form (first steps)))
      `(let (,values ,result)
         (labels ((make-assertion (&optional ,reason ,stacks)
                    (make-instance (funcall ,class-fn ,result ,reason)
                                   :form ',form
                                   :steps ',(nreverse steps)
                                   :args ',(if (consp expanded-form)
                                               (rest expanded-form)
                                               nil)
                                   :values ,values
                                   :reason ,reason
                                   :stacks ,stacks
                                   :desc ,desc))
                  (main ()
                    ,@(cond
                        ((or (not (consp expanded-form))
                             (special-operator-p (first expanded-form))
                             (macro-function (first expanded-form)))
                         `((setf ,values nil)
                           (setf ,result ,expanded-form)))
                        (t
                         `((setf ,values (list ,@(rest expanded-form)))
                           (setf ,result (apply ',(first expanded-form) ,values)))))
                    (record *stats* (make-assertion))
                    ,result))
           (if *debug-on-error*
               (main)
               (block nil
                 (handler-bind ((error
                                  (lambda (,e)
                                    (record *stats* (make-assertion ,e (dissect:stack)))
                                    (return nil))))
                   (main)))))))))

(defmacro ok (form &optional desc)
  `(%okng ,form ,desc
          (lambda (result error)
            (declare (ignore error))
            (if result
                'passed-assertion
                'failed-assertion))))

(defmacro ng (form &optional desc)
  `(%okng ,form ,desc
          (lambda (result error)
            (cond
              (error 'failed-assertion)
              (result 'failed-assertion)
              (t 'passed-assertion)))))

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
