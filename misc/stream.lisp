(in-package #:cl-user)
(defpackage #:rove/misc/stream
  (:use #:cl
        #:trivial-gray-streams)
  (:export #:make-indent-stream
           #:stream-indent-level
           #:with-indent))
(in-package #:rove/misc/stream)

(defclass indent-stream (trivial-gray-stream-mixin
                         fundamental-character-output-stream)
  ((stream :initarg :stream
           :accessor stream-real-stream)
   (level :initarg :level
          :initform 0
          :accessor stream-indent-level)

   (line-column :initform 0
                :accessor indent-stream-line-column)
   (fresh-line-p :initform t
                 :accessor stream-fresh-line-p)))

(defun make-indent-stream (stream)
  (make-instance 'indent-stream :stream stream))

(defmacro with-indent ((stream level) &body body)
  (let ((g-level (gensym "LEVEL"))
        (g-stream (gensym "STREAM")))
    `(let ((,g-level ,level)
           (,g-stream ,stream))
       (incf (stream-indent-level ,g-stream) ,g-level)
       (unwind-protect (progn ,@body)
         (decf (stream-indent-level ,g-stream) ,g-level)))))

(defun new-line-char-p (char)
  (or (char= char #\Newline)
      (char= char #\Linefeed)))

(defmethod stream-write-char ((stream indent-stream) char)
  (cond
    ((new-line-char-p char)
     (write-char char (stream-real-stream stream))
     (setf (stream-fresh-line-p stream) t)
     (setf (indent-stream-line-column stream) 0))
    ((stream-fresh-line-p stream)
     (write-string
      (make-string (stream-indent-level stream) :initial-element #\Space)
      (stream-real-stream stream))
     (write-char char (stream-real-stream stream))
     (setf (stream-fresh-line-p stream) nil)
     (setf (indent-stream-line-column stream) (1+ (stream-indent-level stream))))
    (t
     (write-char char (stream-real-stream stream))
     (incf (indent-stream-line-column stream)))))

(defmethod stream-line-column ((stream indent-stream))
  (if (stream-fresh-line-p stream)
      (stream-indent-level stream)
      (indent-stream-line-column stream)))

(defmethod stream-start-line-p ((stream indent-stream))
  (stream-fresh-line-p stream))

(defmethod stream-finish-output ((stream indent-stream))
  (stream-finish-output (stream-real-stream stream)))

(defmethod stream-force-output ((stream indent-stream))
  (stream-force-output (stream-real-stream stream)))

(defmethod stream-clear-output ((stream indent-stream))
  (stream-clear-output (stream-real-stream stream)))
