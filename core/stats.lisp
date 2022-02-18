(in-package #:cl-user)
(defpackage #:rove/stats
  (:nicknames #:rove/core/stats)
  (:use #:cl
        #:rove/core/result)
  (:export #:*stats*
           #:stats
           #:stats-plan
           #:stats-context
           #:stats-result
           #:stats-context-labels
           #:context-test-count
           #:record
           #:plan
           #:test-begin
           #:test-finish
           #:with-context
           #:suite-begin
           #:suite-finish
           #:system-tests-begin
           #:system-tests-finish
           #:initialize
           #:summarize
           #:toplevel-stats-p
           #:all-failed-assertions))
(in-package #:rove/core/stats)

(defvar *stats* nil)

(defclass stats ()
  ((passed :initform (make-array 0 :adjustable t :fill-pointer 0)
           :accessor stats-passed-tests)
   (failed :initform (make-array 0 :adjustable t :fill-pointer 0)
           :accessor stats-failed-tests)
   (pending :initform (make-array 0 :adjustable t :fill-pointer 0)
            :accessor stats-pending-tests)
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
            (length (stats-passed-tests stats))
            (length (stats-failed-tests stats)))))

(defmethod passed-tests ((object stats))
  (coerce (stats-passed-tests object) 'list))

(defmethod failed-tests ((object stats))
  (coerce (stats-failed-tests object) 'list))

(defmethod pending-tests ((object stats))
  (coerce (stats-pending-tests object) 'list))

(defgeneric stats-context (stats)
  (:documentation "Returns the current stats to record.")
  (:method ((stats stats))
    (or (first (slot-value stats 'contexts))
        stats)))

(defun stats-result (stats)
  (if (/= (length (stats-failed-tests stats)) 0)
      (aref (stats-failed-tests stats) 0)
      (aref (stats-passed-tests stats) 0)))

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
    (+ (length (stats-failed-tests context))
       (length (stats-passed-tests context))
       (length (stats-pending-tests context)))))

(defun new-context (stats test-name)
  (let ((context (make-instance 'stats :name test-name)))
    (push context (slot-value stats 'contexts))
    context))

(defun leave-context (stats)
  (pop (slot-value stats 'contexts)))

(defgeneric record (stats object)
  (:method ((stats stats) (object passed))
    (vector-push-extend object (stats-passed-tests (stats-context stats))))
  (:method ((stats stats) (object failed))
    (vector-push-extend object (stats-failed-tests (stats-context stats))))
  (:method ((stats stats) (object pending))
    (vector-push-extend object (stats-pending-tests (stats-context stats))))
  (:method ((stats null) object)
    (declare (ignore object))))

(defun plan (count)
  (check-type count integer)
  (setf (stats-plan (stats-context *stats*)) count))

(defgeneric test-begin (stats test-name &optional count)
  (:method (stats test-name &optional count)
    (declare (ignore stats test-name count))))

(defgeneric test-finish (stats test-name)
  (:method (stats test-name)
    (declare (ignore stats test-name))))

(defmethod passedp ((stats stats))
  (and (= 0 (length (stats-failed-tests stats)))
       (or (null (stats-plan stats))
           (= (stats-plan stats)
              (+ (length (stats-failed-tests stats))
                 (length (stats-passed-tests stats))
                 (length (stats-pending-tests stats)))))))

(defmacro with-context ((context &key name) &body body)
  (let ((passedp (gensym "PASSEDP"))
        (test (gensym "TEST")))
    `(let ((,context (new-context *stats* ,name)))
       (declare (ignorable ,context))
       (unwind-protect (progn ,@body)
         (let* ((,context (stats-context *stats*))
                (,passedp (and (= 0 (length (stats-failed-tests ,context)))
                               (or (null (stats-plan ,context))
                                   (= (stats-plan ,context)
                                      (+ (length (stats-failed-tests ,context))
                                         (length (stats-passed-tests ,context))
                                         (length (stats-pending-tests ,context)))))))
                (,test
                  (make-instance (if ,passedp
                                     'passed-test
                                     'failed-test)
                                 :name ,name
                                 :passed (coerce (stats-passed-tests (stats-context *stats*)) 'list)
                                 :failed (coerce (stats-failed-tests (stats-context *stats*)) 'list)
                                 :pending (coerce (stats-pending-tests (stats-context *stats*)) 'list))))
           (leave-context *stats*)
           (record *stats* ,test))))))

(defgeneric suite-begin (stats suite-name)
  (:method (stats suite-name)
    (declare (ignore stats suite-name))))

(defgeneric suite-finish (stats suite-name)
  (:method (stats suite-name)
    (declare (ignore stats suite-name))))

(defgeneric system-tests-begin (stats system)
  (:method (stats system)
    (declare (ignore stats system))))

(defgeneric system-tests-finish (stats system)
  (:method (stats system)
    (declare (ignore stats system))))

(defgeneric initialize (stats)
  (:method (stats)
    (declare (ignore stats)))
  (:method :before (stats)
    (with-slots (passed failed pending plan contexts) stats
      (setf passed (make-array 0 :adjustable t :fill-pointer 0)
            failed (make-array 0 :adjustable t :fill-pointer 0)
            pending (make-array 0 :adjustable t :fill-pointer 0)
            plan nil
            contexts nil))))

(defgeneric summarize (stats)
  (:method (stats)
    (declare (ignore stats))))

(defun toplevel-stats-p (stats)
  (null (slot-value stats 'contexts)))

(defun all-failed-assertions (stats)
  (loop for context in (cons stats
                             (slot-value stats 'contexts))
        append (coerce (stats-failed-tests context) 'list)))
