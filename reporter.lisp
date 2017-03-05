(in-package #:cl-user)
(defpackage #:rove/reporter
  (:use #:cl)
  (:export #:reporter
           #:reporter-stream
           #:call-with-reporter
           #:with-reporter))
(in-package #:rove/reporter)

(defvar *report-stream* (make-synonym-stream '*standard-output*))

(defclass reporter ()
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
      #-quicklisp (asdf:load-system (string-downcase package-name)))
    (make-instance
     (intern (format nil "~A-~A" style '#:reporter) package)
     :stream stream)))

(defgeneric call-with-reporter (reporter fn))

(defmacro with-reporter (reporter-style &body body)
  `(call-with-reporter (make-reporter ,reporter-style) (lambda () ,@body)))
