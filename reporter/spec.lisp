(in-package #:cl-user)
(defpackage #:rove/reporter/spec
  (:use #:cl
        #:rove/core/stats
        #:rove/core/result
        #:rove/reporter
        #:rove/misc/stream
        #:rove/misc/color)
  (:import-from #:dissect
                #:present-object)
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

(defmethod record ((reporter spec-reporter) (object pending-assertion))
  (call-next-method)
  (let ((stream (reporter-stream reporter)))
    (fresh-line stream)
    (princ (color-text :aqua "- ") stream)
    (with-indent (stream +2)
      (princ (color-text :aqua (assertion-description object)) stream)
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
  (multiple-value-bind (passedp context) (call-next-method)
    (let ((stream (reporter-stream reporter))
          (test-count (context-test-count context)))
      (decf (stream-indent-level stream) 2)
      (when (toplevel-stats-p reporter)
        (fresh-line stream)
        (write-char #\Newline stream)
        (if (= 0 (length (stats-failed context)))
            (princ
             (color-text :green
                         (format nil "✓ ~D tests completed"
                                 (length (stats-passed context))))
             stream)
            (progn
              (princ
               (color-text :red
                           (format nil "× ~D of ~D tests failed"
                                   (length (stats-failed context))
                                   test-count))
               stream)
              (let ((failed-assertions
                      (labels ((assertions (object)
                                 (typecase object
                                   (failed-assertion (list object))
                                   (failed-test
                                    (apply #'append
                                           (mapcar #'assertions
                                                   (test-failed-assertions object)))))))
                        (loop for object across (stats-failed context)
                              append (assertions object)))))
                (let ((*print-circle* t)
                      (*print-assertion* t))
                  (loop for i from 0
                        for f in failed-assertions
                        do (fresh-line stream)
                           (write-char #\Newline stream)
                           (princ
                            (color-text :white
                                        (format nil "~A) ~A" i (assertion-description f)))
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
                               (fresh-line stream))
                             (with-indent (stream +2)
                               (princ
                                (color-text :gray (princ-to-string f))
                                stream)
                               (fresh-line stream)
                               (when (assertion-stacks f)
                                 (write-char #\Newline stream)
                                 (loop repeat 15
                                       for stack in (assertion-stacks f)
                                       do (princ (color-text :gray (dissect:present-object stack nil)) stream)
                                          (fresh-line stream))))))))))
        (fresh-line stream)
        (unless (= 0 (length (stats-pending context)))
          (princ
           (color-text :aqua
                       (format nil "● ~D tests skipped"
                               (length (stats-pending context))))
           stream)
          (fresh-line stream)))
      (when (and (stats-plan context)
                 (/= (stats-plan context) test-count))
        (princ
         (color-text :red
                     (format nil "× Looks like you planned ~D test~:*~P but ran ~A."
                             (stats-plan context)
                             test-count))
         stream)
        (fresh-line stream))
      passedp)))
