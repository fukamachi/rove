(in-package #:cl-user)
(defpackage #:rove/reporter
  (:use #:cl)
  (:import-from #:rove/core/stats
                #:stats
                #:*stats*)
  (:import-from #:bordeaux-threads)
  (:export #:reporter
           #:reporter-stream
           #:print-message
           #:diag
           #:with-reporter
           #:use-reporter))
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
      (setf package (find-package (intern (string-upcase package-name) "KEYWORD"))))
    (make-instance
     (intern (string-upcase (format nil "~A-~A" style '#:reporter)) package)
     :stream stream)))

(defgeneric print-message (reporter desc)
  (:method ((reporter reporter) desc)
    (princ desc (reporter-stream reporter))))

(defun diag (desc)
  (when (typep *stats* 'reporter)
    (print-message *stats* desc)))

(defmacro with-reporter (reporter-style &body body)
  `(let* ((*stats* (make-reporter ,reporter-style))
          (bt:*default-special-bindings*
            (append `((*stats* . ,*stats*))
                    bt:*default-special-bindings*)))
     ,@body))

(defun use-reporter (style)
  (setf *stats* (make-reporter style)))
