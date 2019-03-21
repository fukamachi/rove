(in-package #:cl-user)
(defpackage #:rove/core/assertion
  (:use #:cl
        #:rove/core/stats
        #:rove/core/result)
  (:import-from #:rove/core/suite/package
                #:wrap-if-toplevel)
  (:import-from #:dissect
                #:stack)
  (:export #:*debug-on-error*
           #:ok
           #:ng
           #:signals
           #:outputs
           #:expands
           #:pass
           #:fail
           #:skip))
(in-package #:rove/core/assertion)

(defvar *debug-on-error*
  (let ((debug-on-error-symbol (intern (string :*rove-debug-on-error*) :cl-user)))
    (and (boundp debug-on-error-symbol)
	 (symbol-value debug-on-error-symbol))))

(defun form-steps (form)
  (if (consp form)
      (let ((steps '()))
        (do ((step-form form (macroexpand-1 step-form)))
            ((or (eq (symbol-package (first step-form)) (find-package :cl))
                 (special-operator-p (first step-form))
                 (not (macro-function (first step-form)))
                 (get (first step-form) 'assertion))
             (cons step-form steps))
          (push step-form steps)))
      (list form)))

(defparameter *fail* '#:interrupt-with-error)
(defmacro form-inspect (form &environment env)
  (let ((args-symbols (gensym "ARGS-SYMBOLS"))
        (args-values (gensym "ARGS-VALUES"))
        (result (gensym "RESULT"))
        (res (gensym))
        (symbs (gensym))
        (vals (gensym)))
    (if (and (consp form)
             (symbolp (first form))
             (not (special-operator-p (first form)))
             (not (macro-function (first form) env)))
        `(let* (,args-symbols
                ,args-values
                (,result
                  (list
                   ,@(loop for f in (rest form)
                           collect
                           `(multiple-value-bind (,res ,symbs ,vals)
                                (form-inspect ,f)
                              (setf ,args-symbols (append ,args-symbols ,symbs (list ',f)))
                              (setf ,args-values (append ,args-values ,vals (list ,res)))
                              ,res)))))
           (values (if (find *fail* ,result :test 'eq)
                       *fail*
                       (apply #',(first form) ,result))
                   ,args-symbols
                   ,args-values))
        `(values ,form nil nil))))

(defmacro %okng (form desc class-fn positive)
  (let ((args (gensym "ARGS"))
        (values (gensym "VALUES"))
        (result (gensym "RESULT"))
        (reason (gensym "REASON"))
        (stacks (gensym "STACKS"))
        (e (gensym "E"))
        (start (gensym "START"))
        (duration (gensym "DURATION"))
        (res (gensym))
        (symbs (gensym))
        (vals (gensym)))
    (let* ((steps (form-steps form))
           (expanded-form (first steps)))
      `(let (,args ,values ,result ,duration ,reason ,stacks)
         (labels ((make-assertion (&optional ,reason ,stacks)
                    (make-instance (funcall ,class-fn (if (eq ,result *fail*)
                                                          ,(not positive)
                                                          (not (null ,result))) ,reason)
                                   :form ',form
                                   :steps ',(nreverse steps)
                                   :args ,args
                                   :values ,values
                                   :reason ,reason
                                   :duration ,duration
                                   :stacks ,stacks
                                   :labels (and *stats*
                                                (stats-context-labels *stats*))
                                   :desc ,desc
                                   :negative ,(not positive)))
                  (main ()
                    (let ((,start (get-internal-real-time)))
                      (multiple-value-bind (,res ,symbs ,vals)
                          (restart-case
                              (form-inspect ,expanded-form)
                            (continue () *fail*))
                        (setf ,result ,res)
                        (setf ,values ,vals)
                        (setf ,args ,symbs))
                      (setf ,duration (- (get-internal-real-time) ,start)))
                    (if (eq ,result *fail*)
                        (progn
                          (record *stats* (make-assertion ,reason ,stacks))
                          nil)
                        (progn
                          (record *stats* (make-assertion))
                          ,result))))
           (if (or *debug-on-error*
                   (toplevel-stats-p *stats*))
               (main)
               (handler-bind ((error
                                (lambda (,e)
                                  (setf ,reason ,e)
                                  (setf ,stacks (dissect:stack))
                                  (let ((restart (find-restart 'continue)))
                                    (when restart
                                      (invoke-restart restart))))))
                 (main))))))))

(defmacro ok (form &optional desc)
  `(wrap-if-toplevel
    (%okng ,form ,desc
           (lambda (result error)
             (declare (ignore error))
             (if result
                 'passed-assertion
                 'failed-assertion))
           t)))

(defmacro ng (form &optional desc)
  `(wrap-if-toplevel
    (%okng ,form ,desc
           (lambda (result error)
             (cond
               (error 'failed-assertion)
               (result 'failed-assertion)
               (t 'passed-assertion)))
           nil)))

(defmacro signals (form &optional (condition ''error))
  "Returns t if given form raise condition of given type,
   and nil otherwise.

   Warning:
   Use only in conjunction with `ok' macro to build an assertion."
  (let ((c (gensym))
        (condition-type (gensym)))
    `(let ((,condition-type ,condition))
       (typep (block nil
                (handler-bind ((condition
                                 (lambda (,c)
                                   (when (typep ,c ,condition-type)
                                     (return ,c)))))
                  ,form
                  nil))
              ,condition-type))))
(setf (get 'signals 'assertion) t)

(defun unwrap-quoted (value)
  (if (and (consp value)
           (eq (first value) 'quote))
      (second value)
      value))

(defmethod form-description ((function (eql 'signals)) args values &key negative)
  (format nil "Expect ~W~:[~; not~] to signal ~A."
          (first args)
          negative
          (or (unwrap-quoted (second args)) 'cl:error)))

(defmacro output-of (form &optional (stream '*standard-output*))
  `(with-output-to-string (,stream)
     ,form))

(defmacro outputs (form content &optional (stream '*standard-output*))
  `(equal (output-of ,form ,stream) ,content))
(setf (get 'outputs 'assertion) t)

(defmethod form-description ((function (eql 'outputs)) args values &key negative)
  (format nil "Expect ~W~:[~; not~] to output ~S."
          (first args)
          negative
          (second args)))

(defun equal* (x y)
  (or (equal x y)
      (and (consp x) (consp y)
           (loop for (x1 . xs) on x
                 for (y1 . ys) on y
                 unless (or (equal* x1 y1)
                            (and (symbolp x1) (symbolp y1)
                                 (null (symbol-package x1))
                                 (null (symbol-package y1))))
                   do (return nil)
                 when (and (null xs) (null ys))
                   do (return t)))))

(defmacro expands (form expanded-form &optional env)
  `(equal* (macroexpand-1 ,form ,env) ,expanded-form))
(setf (get 'expands 'assertion) t)

(defmethod form-description ((function (eql 'expands)) args values &key negative)
  (declare (ignore values))
  (format nil "Expect ~W~:[~; not~] to be expanded to ~W."
          (first args)
          negative
          (second args)))

(defun pass (desc)
  (wrap-if-toplevel
   (record *stats*
           (make-instance 'passed-assertion
                          :form t
                          :desc desc))
   t))

(defun fail (desc)
  (wrap-if-toplevel
   (record *stats*
           (make-instance 'failed-assertion
                          :form t
                          :desc desc))
   nil))

(defun skip (desc)
  (wrap-if-toplevel
   (record *stats*
           (make-instance 'pending-assertion
                          :form t
                          :desc desc))
   t))
