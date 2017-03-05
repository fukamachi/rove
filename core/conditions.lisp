(in-package #:cl-user)
(defpackage #:rove/core/conditions
  (:use #:cl)
  (:export #:assertion
           #:passed
           #:failed
           #:assertion-form
           #:assertion-steps
           #:assertion-args
           #:assertion-values
           #:assertion-reason
           #:assertion-description
           #:assertion-detailed-report

           #:test-begin
           #:test-passed
           #:test-failed
           #:test-name
           #:test-passed-assertions
           #:test-failed-assertions

           #:package-tests-begin
           #:package-tests-passed
           #:package-tests-failed
           #:package-tests-name
           #:package-tests-count))
(in-package #:rove/core/conditions)

(define-condition assertion ()
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

(defun assertion-description (assertion)
  (with-slots (desc form) assertion
    (or desc
        ;; Default description
        ;; TODO: This can be more human-friendly.
        (let ((*print-pretty* nil))
          (format nil "Assertion ~A" form)))))

(define-condition passed (assertion) ()
  (:report
   (lambda (condition stream)
     (princ (assertion-description condition) stream))))

;; TODO: show the test file and file position.
(define-condition failed (assertion) ()
  (:report
   (lambda (condition stream)
     (with-slots (steps args values) condition
       (format stream "~{~A~^~%~}" steps)
       (loop for arg in args
             for value in values
             do (format stream "~&    ~A = ~S" arg value))))))

(define-condition test ()
  ((name :initarg :name
         :reader test-name)))

(define-condition test-result (test)
  ((passed :initarg :passed
           :initform nil
           :reader test-passed-assertions)
   (failed :initarg :failed
           :initform nil
           :reader test-failed-assertions))
  (:report (lambda (condition stream)
             (with-slots (name passed failed) condition
               (format stream "Test ~S ~:[failed~;passed~] (~D/~D)"
                       name
                       (null failed)
                       (length passed)
                       (+ (length failed) (length passed)))))))

(define-condition test-begin (test) ())
(define-condition test-passed (test-result) ())
(define-condition test-failed (test-result) ())

(define-condition package-tests ()
  ((name :initarg :name
         :reader package-tests-name)))

(define-condition package-tests-begin (package-tests)
  ((count :initarg :count
          :reader package-tests-count)))

(define-condition package-tests-passed (package-tests) ())
(define-condition package-tests-failed (package-tests) ())
