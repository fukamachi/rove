(in-package #:cl-user)
(defpackage #:rove/core/stats
  (:use #:cl
        #:rove/core/result)
  (:export #:*stats*
           #:stats
           #:stats-passed
           #:stats-failed
           #:stats-pending
           #:stats-plan
           #:stats-context
           #:stats-context-labels
           #:context-test-count
           #:record
           #:plan
           #:test-begin
           #:test-finish
           #:toplevel-stats-p
           #:all-failed-assertions))
(in-package #:rove/core/stats)

(defvar *stats* nil)

(defclass stats ()
  ((passed :initform (make-array 0 :adjustable t :fill-pointer 0)
           :accessor stats-passed)
   (failed :initform (make-array 0 :adjustable t :fill-pointer 0)
           :accessor stats-failed)
   (pending :initform (make-array 0 :adjustable t :fill-pointer 0)
            :accessor stats-pending)
   (plan :initarg :plan
         :initform nil
         :accessor stats-plan)

   (name :initarg :name
         :initform nil
         :accessor stats-name)

   ;; TODO: lock
   (contexts :initform nil)))

(defmethod print-object ((stats stats) stream)
  (print-unreadable-object (stats stream :type t)
    (format stream "PASSED=~D, FAILED=~D"
            (length (stats-passed stats))
            (length (stats-failed stats)))))

(defgeneric stats-context (stats)
  (:documentation "Returns the current stats to record.")
  (:method ((stats stats))
    (or (first (slot-value stats 'contexts))
        stats)))

(defgeneric stats-context-labels (stats)
  (:documentation "Returns the labels of the current contexts (including nested ones)")
  (:method ((stats stats))
    (cdr
     (reduce (lambda (labels label)
               (cons label labels))
             (slot-value stats 'contexts)
             :key #'stats-name
             :initial-value nil))))

(defgeneric context-test-count (context)
  (:method ((context stats))
    (+ (length (stats-failed context))
       (length (stats-passed context))
       (length (stats-pending context)))))

(defun new-context (stats test-name)
  (let ((context (make-instance 'stats :name test-name)))
    (push context (slot-value stats 'contexts))
    context))

(defun leave-context (stats)
  (pop (slot-value stats 'contexts)))

(defgeneric record (stats object)
  (:method ((stats stats) (object passed))
    (vector-push-extend object (stats-passed (stats-context stats))))
  (:method ((stats stats) (object failed))
    (vector-push-extend object (stats-failed (stats-context stats))))
  (:method ((stats stats) (object pending))
    (vector-push-extend object (stats-pending (stats-context stats))))
  (:method ((stats null) object)
    (declare (ignore object))))

(defun plan (count)
  (check-type count integer)
  (setf (stats-plan (stats-context *stats*)) count))

(defgeneric test-begin (stats test-name &optional count)
  (:method (stats test-name &optional count)
    (declare (ignore test-name))
    (new-context stats test-name)
    (setf (stats-plan (stats-context stats)) count)
    (values)))

(defgeneric test-finish (stats test-name)
  (:method (stats test-name)
    (let* ((context (stats-context stats))
           (passedp (and (= 0 (length (stats-failed context)))
                         (or (null (stats-plan context))
                             (= (stats-plan context)
                                (+ (length (stats-failed context))
                                   (length (stats-passed context))
                                   (length (stats-pending context)))))))
           (test
             (make-instance (if passedp
                                'passed-test
                                'failed-test)
                            :name test-name
                            :passed (coerce (stats-passed (stats-context stats)) 'list)
                            :failed (coerce (stats-failed (stats-context stats)) 'list)
                            :pending (coerce (stats-pending (stats-context stats)) 'list))))
      (let ((context (leave-context stats)))
        (record stats test)

        (values passedp context)))))

(defun toplevel-stats-p (stats)
  (null (slot-value stats 'contexts)))

(defun all-failed-assertions (stats)
  (loop for context in (cons stats
                             (slot-value stats 'contexts))
        append (coerce (stats-failed context) 'list)))
