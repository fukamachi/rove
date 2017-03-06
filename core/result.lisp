(in-package #:cl-user)
(defpackage #:rove/core/result
  (:use #:cl)
  (:export #:passed
           #:failed

           #:assertion
           #:assertion-form
           #:assertion-steps
           #:assertion-args
           #:assertion-values
           #:assertion-reason
           #:assertion-description
           #:dump-details

           #:test
           #:test-passed-assertions
           #:test-failed-assertions
           #:test-passed-p

           #:passed-assertion
           #:failed-assertion
           #:passed-test
           #:failed-test))
(in-package #:rove/core/result)

(defclass passed () ())
(defclass failed () ())

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
   (desc :initarg :desc
         :initform nil)))

(defmethod print-object ((assertion assertion) stream)
  (with-slots (steps args values) assertion
    (format stream "~{~S~^~%~}" steps)
    (loop for arg in args
          for value in values
          do (format stream "~&    ~A = ~S" arg value))))

(defun form-description (form values)
  (destructuring-bind (test-fn &rest args) form
    (case test-fn
      (cl:typep (format nil "Expect ~A to be an instance of ~A."
                        (first args)
                        (second values)))
      (cl:not (format nil "Expect ~A to be false." (first args)))
      (otherwise (format nil "Expect ~A to be true." form)))))

(defun assertion-description (assertion)
  (with-slots (desc form values) assertion
    (or desc
        ;; Default description
        (form-description form values))))

(defclass test ()
  ((name :initarg :name
         :reader test-name)
   (passed :initarg :passed
           :initform nil
           :accessor test-passed-assertions)
   (failed :initarg :failed
           :initform nil
           :accessor test-failed-assertions)))

(defgeneric test-passed-p (test)
  (:method ((test test))
    (= 0 (length (test-failed-assertions test)))))

(defclass passed-assertion (assertion passed) ())
(defclass failed-assertion (assertion failed) ())

(defclass passed-test (test passed) ())
(defclass failed-test (test failed) ())
