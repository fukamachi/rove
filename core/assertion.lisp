(in-package #:cl-user)
(defpackage #:rove/core/assertion
  (:use #:cl
        #:rove/core/stats
        #:rove/core/result)
  (:shadow #:continue)
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

(defparameter *reason* nil)
(defparameter *stacks* nil)

(defun error-handler (e)
  (setf *reason* e)
  (setf *stacks* (dissect:stack))
  (let ((restart (find-restart 'continue)))
    (when restart
      (invoke-restart restart))))

(defun ignore-handler (e)
  (declare (ignore e)))

(defun detect-error-handler ()
  (if (or *debug-on-error*
          (toplevel-stats-p *stats*))
      #'ignore-handler
      #'error-handler))

(defun %okng-record (form result args-symbols args-values steps duration desc class-fn positive)
  (let ((assertion
          (make-instance (funcall class-fn
                                  (if (eq result *fail*)
                                      (not positive)
                                      (not (null result)))
                                  *reason*)
                         :form form
                         :steps (reverse steps)
                         :args args-symbols
                         :values args-values
                         :reason *reason*
                         :duration duration
                         :stacks *stacks*
                         :labels (and *stats*
                                      (stats-context-labels *stats*))
                         :negative (not positive))))
    (record *stats* assertion)
    result))

(defun calc-duration (start)
  (truncate (- (get-internal-real-time) start)
            (/ internal-time-units-per-second 1000)))

(defmacro %okng (form desc class-fn positive &environment env)
  (let* ((form-steps (form-steps form))
         (expanded-form (first form-steps))
         (result (gensym "RESULT"))
         (args-symbols (gensym "ARGS-SYMBOLS"))
         (args-values (gensym "ARGS-VALUES"))
         (steps (gensym "STEPS"))
         (start (gensym "START")))
    `(let ((,start (get-internal-real-time))
           (,steps ',form-steps)
           *stacks* *reason*)
       (handler-bind ((error (detect-error-handler)))
         (multiple-value-bind (,result ,args-symbols ,args-values)
             (restart-case
                 (form-inspect ,expanded-form)
               (continue () *fail*))
           (%okng-record ',expanded-form
                         ,result
                         ,args-symbols
                         ,args-values
                         ,steps
                         ;; Duration is in milliseconds.
                         (calc-duration ,start)
                         ,desc
                         ,class-fn
                         ,positive))))))

(defun ok-assertion-class (result error)
  (declare (ignore error))
  (if result
    'passed-assertion
    'failed-assertion))

(defmacro ok (form &optional desc)
  `(%okng ,form ,desc
          #'ok-assertion-class
          t))

(defun ng-assertion-class (result error)
  (cond
    (error 'failed-assertion)
    (result 'failed-assertion)
    (t 'passed-assertion)))

(defmacro ng (form &optional desc)
  `(%okng ,form ,desc
          #'ng-assertion-class
          nil))

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
(setf (get 'outputs 'assertion) nil)

(defmethod form-description ((function (eql 'outputs)) args values &key negative)
  (format nil "Expect ~W~:[~; not~] to output ~S."
          (first args)
          negative
          (second args)))

(defun equal* (x y)
  (flet ((equal-or-both-uninterned-symbol-p (x1 y1)
	   (or (equal x1 y1)
	       (and (symbolp x1) (symbolp y1)
                    (null (symbol-package x1))
                    (null (symbol-package y1))))))
    (tree-equal x y :test #'equal-or-both-uninterned-symbol-p)))

(defmacro expands (form expanded-form &optional env)
  `(equal* (macroexpand-1 ,form ,env) ,expanded-form))
(setf (get 'expands 'assertion) nil)

(defmethod form-description ((function (eql 'expands)) args values &key negative)
  (declare (ignore values))
  (format nil "Expect ~W~:[~; not~] to be expanded to ~W."
          (first args)
          negative
          (second args)))

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
