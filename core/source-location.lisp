(defpackage #:rove/core/source-location
  (:use #:cl)
  (:export #:source-location
           #:source-location-file-position))
(in-package #:rove/core/source-location)

(defvar *sf-index*)
(defvar *target-sf-index*)
(defvar *newline-count*)
(defvar *bol-pos*)

(define-condition form-found () ())

(defun index-read-list (stream char)
  (incf *sf-index*)
  (when (= *sf-index* *target-sf-index*)
    (unread-char char stream)
    (signal 'form-found))
  (funcall '#.(get-macro-character #\() stream char))

(defun count-newline (stream char)
  (declare (ignore char))
  (incf *newline-count*)
  (setf *bol-pos* (file-position stream))
  (values))

(defparameter *newline-count-readtable*
  (let ((readtable (copy-readtable)))
    (set-macro-character #\Newline #'count-newline nil readtable)
    readtable))

(defparameter *location-finder-readtable*
  (let ((readtable (copy-readtable)))
    (set-macro-character #\( #'index-read-list nil readtable)
    (set-macro-character #\Newline #'count-newline nil readtable)
    readtable))

(defun get-file-position (file tlf-index sf-index)
  (let ((*newline-count* 0)
        (*bol-pos* 0)
        (*readtable* (copy-readtable *newline-count-readtable*)))
    (with-open-file (in file)
      (dotimes (i tlf-index)
        (read in))
      (handler-case
          (let ((*readtable* (copy-readtable *location-finder-readtable*))
                (*sf-index* 0)
                (*target-sf-index* (1+ sf-index)))
            (read in))
        (form-found ()
          ;; Return the line number and the column.
          (values (1+ *newline-count*)
                  (- (file-position in) *bol-pos*)))
        (end-of-file ())))))

(defun source-location ()
  #-sbcl nil
  #+sbcl
  (let ((sb-source (sb-c::make-definition-source-location)))
    (when (and sb-source
               (sb-c::definition-source-location-namestring sb-source))
      (list
       :file (sb-c:definition-source-location-namestring sb-source)
       :tlf-index (sb-c:definition-source-location-toplevel-form-number sb-source)
       :sf-index (sb-c:definition-source-location-form-number sb-source)))))

(defun source-location-file-position (source-location)
  (when source-location
    (destructuring-bind (&key file tlf-index sf-index)
        source-location
      (multiple-value-bind (line column)
          (get-file-position file tlf-index sf-index)
        (list file line column)))))
