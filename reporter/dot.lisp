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
  (let ((stream (reporter-stream reporter))
        (color
          (if (and (assertion-duration object)
                   (< (/ 75 2) (assertion-duration object)))
              :yellow
              :gray)))
    (princ (color-text color ".") stream)))

(defmethod record :after ((reporter dot-reporter) (object failed-assertion))
  (let ((stream (reporter-stream reporter)))
    (princ (color-text :red ".") stream)))

(defmethod record :after ((reporter dot-reporter) (object pending-assertion))
  (let ((stream (reporter-stream reporter)))
    (princ (color-text :aqua ".") stream)))

(defmethod test-finish ((reporter dot-reporter) description)
  (declare (ignore description))
  (let ((context (stats-context reporter)))
    (passedp context)))

(defmethod summarize ((reporter dot-reporter))
  (when (toplevel-stats-p reporter)
    (format-failure-tests (reporter-stream reporter)
                          (stats-passed-tests reporter)
                          (stats-failed-tests reporter)
                          (stats-pending-tests reporter))))
