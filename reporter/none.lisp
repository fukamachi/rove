(in-package #:cl-user)
(defpackage #:rove/reporter/none
  (:use #:cl
        #:rove/reporter)
  (:export #:none-reporter))
(in-package #:rove/reporter/none)

(defclass none-reporter (reporter) ())
