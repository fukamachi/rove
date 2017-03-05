(in-package #:cl-user)
(defpackage #:rove/reporter/color
  (:use #:cl)
  (:import-from #:uiop
                #:getenv)
  (:export #:color-text))
(in-package #:rove/reporter/color)

(defvar *enable-colors*
  (not
   (or (equal (uiop:getenv "EMACS") "t")
       (uiop:getenv "INSIDE_EMACS"))))

(defparameter *color-code*
  `((:red    . 31)
    (:green  . 32)
    (:yellow . 33)
    (:aqua   . 36)
    (:white  . 37)
    (:gray   . 90)))

(defun color-text (color text)
  (if *enable-colors*
      (if (= (length text) 0)
          text
          (let ((code (cdr (assoc color *color-code*))))
            (assert code)
            (format nil "~C[~Am~A~C[0m"
                    #\Esc
                    code
                    text
                    #\Esc)))
      text))
