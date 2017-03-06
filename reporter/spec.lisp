(in-package #:cl-user)
(defpackage #:rove/reporter/spec
  (:use #:cl
        #:rove/core/stats
        #:rove/core/result
        #:rove/reporter
        #:rove/misc/stream
        #:rove/misc/color)
  (:export #:spec-reporter))
(in-package #:rove/reporter/spec)

(defclass spec-reporter (reporter) ())

(defmethod initialize-instance :after ((reporter spec-reporter) &rest initargs &key stream &allow-other-keys)
  (declare (ignore initargs))
  (when stream
    (setf (reporter-stream reporter)
          (make-indent-stream stream))))

(defmethod record ((reporter spec-reporter) (object passed-assertion))
  (call-next-method)
  (let ((stream (reporter-stream reporter)))
    (fresh-line stream)
    (princ (color-text :green "✓ ") stream)
    (with-indent (stream +2)
      (princ (color-text :gray (assertion-description object)) stream)
      (fresh-line stream))))

(defmethod record ((reporter spec-reporter) (object failed-assertion))
  (call-next-method)
  (let ((stream (reporter-stream reporter)))
    (fresh-line stream)
    (princ (color-text :red "× ") stream)
    (with-indent (stream +2)
      (princ
       (color-text :red
                   (format nil "~D) ~A"
                           (1- (length (stats-failed (stats-context reporter))))
                           (assertion-description object)))
       stream)
      (fresh-line stream))))

(defmethod test-begin ((reporter spec-reporter) test-name &optional count)
  (declare (ignore count))
  (call-next-method)
  (let ((stream (reporter-stream reporter)))
    (fresh-line stream)
    (princ (color-text :white test-name) stream)
    (fresh-line stream)
    (incf (stream-indent-level stream) 2)))

(defmethod test-finish ((reporter spec-reporter) test-name)
  (let ((passedp (call-next-method))
        (stream (reporter-stream reporter)))
    (decf (stream-indent-level stream) 2)
    (when (toplevel-stats-p reporter)
      (fresh-line stream)
      (write-char #\Newline stream)
      (if (= 0 (length (stats-failed reporter)))
          (princ
           (color-text :green
                       (format nil "✓ ~D tests completed"
                               (length (stats-passed reporter))))
           stream)
          (progn
            (princ
             (color-text :red
                         (format nil "× ~D of ~D tests failed"
                                 (length (stats-failed reporter))
                                 (+ (length (stats-failed reporter))
                                    (length (stats-passed reporter))))))
            (let ((failed-assertions
                    (labels ((assertions (object)
                               (typecase object
                                 (failed-assertion (list object))
                                 (failed-test
                                  (apply #'append
                                         (mapcar #'assertions
                                                 (test-failed-assertions object)))))))
                      (loop for object across (stats-failed reporter)
                            append (assertions object)))))
              (let ((*print-circle* t))
                (loop for i from 0
                      for f in failed-assertions
                      do (fresh-line stream)
                         (write-char #\Newline stream)
                         (princ
                          (color-text :white
                                      (format nil "~A) ~A failed." i (assertion-description f)))
                          stream)
                         (fresh-line stream)
                         (with-indent (stream (+ (length (write-to-string i)) 2))
                           (when (assertion-reason f)
                             (princ
                              (color-text :red
                                          (format nil "~A: ~A"
                                                  (type-of (assertion-reason f))
                                                  (assertion-reason f)))
                              stream)
                             (fresh-line stream)
                             (write-char #\Newline stream))
                           (with-indent (stream +2)
                             (princ
                              (color-text :gray (princ-to-string f))
                              stream))))))))
      (fresh-line stream))
    passedp))
