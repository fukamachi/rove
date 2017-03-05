(in-package #:cl-user)
(defpackage #:rove/reporter/style/spec
  (:use #:cl
        #:rove/core/conditions
        #:rove/reporter
        #:rove/reporter/stream
        #:rove/reporter/color)
  (:export #:spec-reporter))
(in-package #:rove/reporter/style/spec)

(defclass spec-reporter (reporter) ())

(defmethod initialize-instance :after ((reporter spec-reporter) &rest initargs &key stream &allow-other-keys)
  (declare (ignore initargs))
  (when stream
    (setf (reporter-stream reporter)
          (make-indent-stream stream))))

(defmethod call-with-reporter ((reporter spec-reporter) fn)
  (let ((stream (reporter-stream reporter))
        (count nil)
        (failed '())
        (failed-test-count 0)
        (index 0))
    (handler-bind ((passed
                     (lambda (c)
                       (fresh-line stream)
                       (princ (color-text :green "✓ ") stream)
                       (with-indent (stream +2)
                         (princ (color-text :gray (assertion-description c)) stream)
                         (fresh-line stream))))
                   (failed
                     (lambda (c)
                       (push c failed)
                       (fresh-line stream)
                       (princ (color-text :red "× ") stream)
                       (with-indent (stream +2)
                         (princ
                          (color-text :red
                                      (format nil "~D) ~A"
                                              index
                                              (assertion-description c)))
                          stream)
                         (fresh-line stream))
                       (incf index)))
                   (test-begin
                     (lambda (c)
                       (fresh-line stream)
                       (princ (color-text :white (test-name c)) stream)
                       (fresh-line stream)
                       (incf (stream-indent-level stream) 2)))
                   (test-passed
                     (lambda (c)
                       (declare (ignore c))
                       (decf (stream-indent-level stream) 2)))
                   (test-failed
                     (lambda (c)
                       (declare (ignore c))
                       (incf failed-test-count)
                       (decf (stream-indent-level stream) 2)))
                   (package-tests-begin
                     (lambda (c)
                       (fresh-line stream)
                       (princ (color-text :white (package-tests-name c)) stream)
                       (fresh-line stream)
                       (setf count (package-tests-count c))
                       (incf (stream-indent-level stream) 2)))
                   (package-tests-passed
                     (lambda (c)
                       (declare (ignore c))
                       (decf (stream-indent-level stream) 2)
                       (fresh-line stream)
                       (write-char #\Newline stream)
                       (princ
                        (color-text :green
                                    (format nil "✓ ~D tests completed" count))
                        stream)
                       (fresh-line stream)))
                   (package-tests-failed
                     (lambda (c)
                       (declare (ignore c))
                       (decf (stream-indent-level stream) 2)
                       (fresh-line stream)
                       (write-char #\Newline stream)
                       (princ
                        (color-text :red
                                    (format nil "× ~D of ~D tests failed:"
                                            failed-test-count
                                            count))
                        stream)
                       (fresh-line stream)
                       (let ((*print-circle* t))
                         (loop for i from 0
                               for f in (nreverse failed)
                               do (let ((i (write-to-string i)))
                                    (fresh-line stream)
                                    (write-char #\Newline stream)
                                    (princ
                                     (color-text :white
                                                 (format nil "~A) ~A failed."
                                                         i
                                                         (assertion-description f)))
                                     stream)
                                    (fresh-line stream)
                                    (with-indent (stream (+ (length i) 2))
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
                                         (color-text :gray
                                                     (princ-to-string f))
                                         stream))))))
                       (fresh-line stream))))
      (funcall fn))))
