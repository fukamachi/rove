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
           #:assert-ok
           #:assert-ng
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

(defun %okng-record (form result args-symbols args-values steps stacks reason duration desc class-fn positive source-location)
  (let ((assertion
          (make-instance (funcall class-fn
                                  (if (eq result *fail*)
                                      (not positive)
                                      (not (null result)))
                                  reason)
                         :form form
                         :steps steps
                         :args args-symbols
                         :values args-values
                         :reason reason
                         :desc desc
                         :duration duration
                         :stacks stacks
                         :labels (and *stats*
                                      (stats-context-labels *stats*))
                         :negative (not positive)
                         :source-location source-location)))
    (record *stats* assertion)
    result))

(defun record-error (form steps reason duration description class-fn positive source-location)
  (%okng-record form *fail* nil nil steps (dissect:stack) reason duration description class-fn positive source-location))

(defun calc-duration (start)
  (truncate (- (get-internal-real-time) start)
            (/ internal-time-units-per-second 1000)))

(defun debug-on-error-p ()
  (or *debug-on-error*
      (toplevel-stats-p *stats*)))

(defun source-filename ()
  #-sbcl nil
  #+sbcl
  (let ((sb-source (sb-c::make-definition-source-location)))
    (and sb-source
         (sb-c:definition-source-location-namestring sb-source))))

(defmacro %okng (form desc class-fn positive)
  (let* ((form-steps (form-steps form))
         (form (gensym "FORM"))
         (expanded-form (first form-steps))
         (result (gensym "RESULT"))
         (args-symbols (gensym "ARGS-SYMBOLS"))
         (args-values (gensym "ARGS-VALUES"))
         (steps (gensym "STEPS"))
         (e (gensym "E"))
         (start (gensym "START"))
         (block-label (gensym "BLOCK"))
         (source-location (gensym "SOURCE-LOCATION"))
         (file (source-filename)))
    `(let* ((,start (get-internal-real-time))
            (,form ',expanded-form)
            (,steps ',(reverse form-steps))
            (,source-location
              #-sbcl nil
              #+sbcl
              ,(if file
                   `(cons ,file (multiple-value-list (sb-c::compile-file-line)))
                   nil)))
       (block ,block-label
         (handler-bind
             ((error (lambda (,e)
                       (record-error ,form ,steps ,e (calc-duration ,start) ,desc ,class-fn ,positive ,source-location)
                       (unless (debug-on-error-p)
                         (return-from ,block-label *fail*)))))
           (multiple-value-bind (,result ,args-symbols ,args-values)
               (form-inspect ,expanded-form)
             (%okng-record ,form
                           ,result ,args-symbols ,args-values
                           ,steps
                           nil
                           nil
                           (calc-duration ,start)
                           ,desc
                           ,class-fn
                           ,positive
                           ,source-location)))))))

(defun ok-assertion-class (result error)
  (declare (ignore error))
  (if result
      'passed-assertion
      'failed-assertion))

(defun ng-assertion-class (result error)
  (cond
    (error 'failed-assertion)
    (result 'failed-assertion)
    (t 'passed-assertion)))

(defmacro assert-ok (form &optional desc class-fn)
  `(%okng ,form ,desc ,(or class-fn '#'ok-assertion-class) t))

(defmacro assert-ng (form &optional desc class-fn)
  `(%okng ,form ,desc ,(or class-fn '#'ng-assertion-class) nil))

(defmacro ok (form &optional desc)
  `(assert-ok ,form ,desc))

(defmacro ng (form &optional desc)
  `(assert-ng ,form ,desc))

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
