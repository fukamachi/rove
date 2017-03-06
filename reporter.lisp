(in-package #:cl-user)
(defpackage #:rove/reporter
  (:use #:cl)
  (:import-from #:rove/core/stats
                #:stats
                #:*stats*)
  (:export #:reporter
           #:reporter-stream
           #:with-reporter))
(in-package #:rove/reporter)

(defvar *report-stream* (make-synonym-stream '*standard-output*))

(defclass reporter (stats)
  ((stream :initarg :stream
           :accessor reporter-stream)))

(defun make-reporter (style &key (stream *report-stream*))
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
     :stream stream)))

(defmacro with-reporter (reporter-style &body body)
  `(let ((*stats* (make-reporter ,reporter-style)))
     ,@body))
