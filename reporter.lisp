(in-package #:cl-user)
(defpackage #:rove/reporter
  (:use #:cl)
  (:import-from #:rove/core/stats
                #:stats
                #:*stats*)
  (:import-from #:rove/reporter/registry
                #:get-reporter)
  (:import-from #:bordeaux-threads)
  (:export #:reporter
           #:reporter-stream
           #:*report-stream*
           #:diag
           #:with-reporter
           #:invoke-reporter
           #:use-reporter))
(in-package #:rove/reporter)

(defvar *report-stream* (make-synonym-stream '*standard-output*))

(defclass reporter (stats)
  ((stream :initarg :stream
           :accessor reporter-stream)))

(defun make-reporter (style &key (stream *report-stream*))
  (let ((class-name (get-reporter style)))
    (if class-name
        (make-instance class-name :stream stream)
        (let* ((package-name
                 (format nil "~A/~A"
                         '#:rove/reporter
                         style))
               (package (find-package package-name)))
          (unless package
            #+quicklisp (ql:quickload (string-downcase package-name) :silent t)
            #-quicklisp (asdf:load-system (string-downcase package-name))
            (setf package (find-package package-name)))
          (make-instance
           (intern (format nil "~A-~A" style '#:reporter) package)
           :stream stream)))))

(defgeneric print-message (reporter desc)
  (:method ((reporter reporter) desc)
    (princ desc (reporter-stream reporter))))

(defun diag (desc)
  (when (typep *stats* 'reporter)
    (print-message *stats* desc)))

(defmacro with-reporter (reporter-style &body body)
  `(invoke-reporter (make-reporter ,reporter-style)
                    (lambda () ,@body)))

(defgeneric invoke-reporter (repoter function))
(defmethod invoke-reporter (reporter function)
  (let ((*stats* reporter)
        (bt:*default-special-bindings*
          (append `((*stats* . ,*stats*))
                  bt:*default-special-bindings*)))
    (funcall function)))

(defun use-reporter (style)
  (setf *stats* (make-reporter style)))
