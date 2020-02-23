(defpackage #:rove/reporter/registry
  (:use #:cl)
  (:export #:add-reporter
           #:get-reporter))
(in-package #:rove/reporter/registry)

(defvar *registry* (make-hash-table :test 'equal))

(defun add-reporter (style reporter)
  (setf (gethash (string style) *registry*) reporter))

(defun get-reporter (style)
  (gethash (string style) *registry*))
