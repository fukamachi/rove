(in-package #:cl-user)
(defpackage #:rove/core/result
  (:use #:cl)
  (:export #:passed
           #:failed
           #:pending

           #:form-description

           #:*print-assertion*
           #:assertion
           #:assertion-form
           #:assertion-steps
           #:assertion-args
           #:assertion-values
           #:assertion-reason
           #:assertion-duration
           #:assertion-stacks
           #:assertion-description
           #:dump-details

           #:test
           #:test-name
           #:test-passed-assertions
           #:test-failed-assertions
           #:test-pending-assertions
           #:test-passed-p

           #:passed-assertion
           #:failed-assertion
           #:pending-assertion
           #:passed-test
           #:failed-test))
(in-package #:rove/core/result)

(defvar *print-assertion* nil)

(defclass passed () ())
(defclass failed () ())
(defclass pending () ())

(defclass assertion ()
  ((form :initarg :form
         :reader assertion-form)
   (steps :initarg :steps
          :initform nil
          :reader assertion-steps)
   (args :initarg :args
         :initform nil
         :reader assertion-args)
   (values :initarg :values
           :initform nil
           :reader assertion-values)
   (reason :initarg :reason
           :initform nil
           :reader assertion-reason)
   (duration :initarg :duration
             :initform nil
             :reader assertion-duration)
   (stacks :initarg :stacks
           :initform nil
           :reader assertion-stacks)
   (desc :initarg :desc
         :initform nil)))

(defmethod print-object ((assertion assertion) stream)
  (if *print-assertion*
      (with-slots (steps args values) assertion
        (format stream "~{~S~^~%~}" steps)
        (loop for arg in args
              for value in values
              do (format stream "~&    ~A = ~S" arg value)))
      (call-next-method)))

(defgeneric form-description (function args values)
  (:method (function args values)
    (declare (ignore values))
    (format nil "Expect ~A to be true." `(,function ,@args)))
  (:method ((function (eql 'cl:typep)) args values)
    (format nil "Expect ~A to be an instance of ~A."
            (first args)
            (second values)))
  (:method ((function (eql 'cl:not)) args values)
    (declare (ignore values))
    (format nil "Expect ~A to be false." (first args))))

(defun assertion-description (assertion)
  (with-slots (desc form values) assertion
    (or desc
        ;; Default description
        (if (consp form)
            (form-description (first form) (rest form) values)
            (format nil "Expect ~A to be true." form)))))

(defclass test ()
  ((name :initarg :name
         :reader test-name)
   (passed :initarg :passed
           :initform nil
           :accessor test-passed-assertions)
   (failed :initarg :failed
           :initform nil
           :accessor test-failed-assertions)
   (pending :initarg :pending
            :initform nil
            :accessor test-pending-assertions)))

(defgeneric test-passed-p (test)
  (:method ((test test))
    (= 0 (length (test-failed-assertions test)))))

(defclass passed-assertion (assertion passed) ())
(defclass failed-assertion (assertion failed) ())
(defclass pending-assertion (assertion pending) ())

(defclass passed-test (test passed) ())
(defclass failed-test (test failed) ())
