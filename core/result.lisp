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
           #:assertion-labels
           #:assertion-description
           #:dump-details

           #:test
           #:test-name
           #:passed-tests
           #:failed-tests
           #:pending-tests
           #:passedp

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
   (labels :initarg :labels
           :initform nil
           :reader assertion-labels)
   (desc :initarg :desc
         :initform nil)
   (negative :initarg :negative
             :initform nil)))

(defmethod print-object ((assertion assertion) stream)
  (if *print-assertion*
      (with-slots (steps args values) assertion
        (format stream "~{~S~^~%~}" steps)
        (loop for arg in args
              for value in values
              unless (constantp arg)
                do (format stream "~&    ~A = ~W" arg value)))
      (call-next-method)))

(defgeneric form-description (function args values &key negative)
  (:method (function args values &key negative)
    (declare (ignore values))
    (format nil "Expect ~W to be ~:[true~;false~]." `(,function ,@args) negative))
  (:method ((function (eql 'cl:typep)) args values &key negative)
    (declare (ignore values))
    (let* ((type (second args))
           (type
             (if (and (consp type)
                      (eq (first type) 'quote))
                 (second type)
                 type)))
      (format nil "Expect ~W~:[~; not~] to be an instance of ~A."
              (first args)
              negative
              type)))
  (:method ((function (eql 'cl:not)) args values &key negative)
    (declare (ignore values))
    (format nil "Expect ~W to be ~:[false~;true~]."
            (first args)
            negative)))

(defgeneric assertion-description (assertion)
  (:documentation "Returns a string to print description of
                   an object of class `assertion'.

                   Useful, when you write your own assertions based
                   on Rove's and want to customize their representation
                   in test results."))

(defmethod assertion-description ((assertion t))
  (with-slots (desc form values negative) assertion
    (or desc
        ;; Default description
        (if (consp form)
            (form-description (first form) (rest form) values :negative negative)
            (format nil "Expect ~W to be ~:[true~;false~]." form negative)))))

(defclass test ()
  ((name :initarg :name
         :reader test-name)
   (passed :initarg :passed
           :initform nil
           :accessor test-passed-tests)
   (failed :initarg :failed
           :initform nil
           :accessor test-failed-tests)
   (pending :initarg :pending
            :initform nil
            :accessor test-pending-tests)))

(defmethod print-object ((test test) stream)
  (print-unreadable-object (test stream :type t)
    (format stream "~A (PASSED=~D, FAILED=~D)"
            (test-name test)
            (length (test-passed-tests test))
            (length (test-failed-tests test)))))

(defgeneric passedp (object)
  (:method ((object test))
    (= 0 (length (test-failed-tests object)))))

(defgeneric passed-tests (object)
  (:method ((object test))
    (test-passed-tests object)))
(defgeneric failed-tests (object)
  (:method ((object test))
    (test-failed-tests object)))
(defgeneric pending-tests (object)
  (:method ((object test))
    (test-pending-tests object)))

(defclass passed-assertion (assertion passed) ())
(defclass failed-assertion (assertion failed) ())
(defclass pending-assertion (assertion pending) ())

(defclass passed-test (test passed) ())
(defclass failed-test (test failed) ())
