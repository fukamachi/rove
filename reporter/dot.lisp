(defpackage #:rove/reporter/dot
  (:use #:cl
        #:rove/reporter
        #:rove/core/stats
        #:rove/core/result
        #:rove/misc/stream
        #:rove/misc/color
        #:rove/utils/reporter)
  (:export #:dot-reporter))
(in-package #:rove/reporter/dot)

(defclass dot-reporter (reporter) ())

(defmethod record :after ((reporter dot-reporter) (object passed-assertion))
  (let ((duration (assertion-duration object)))
    (when duration
      (let ((stream (reporter-stream reporter))
            (color
              (if (< (/ 75 2) duration)
                  :yellow
                  :gray)))
        (princ (color-text color ".") stream)))))

(defmethod record :after ((reporter dot-reporter) (object failed-assertion))
  (let ((stream (reporter-stream reporter)))
    (princ (color-text :red ".") stream)))

(defmethod record :after ((reporter dot-reporter) (object pending-assertion))
  (let ((stream (reporter-stream reporter)))
    (princ (color-text :aqua ".") stream)))

(defmethod test-finish ((reporter dot-reporter) test-name)
  (declare (ignore test-name))
  (multiple-value-bind (passedp context) (call-next-method)
    (when (toplevel-stats-p reporter)
      (format-failure-tests (reporter-stream reporter) context))
    passedp))
