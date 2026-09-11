(in-package #:cl-user)
(defpackage #:rove/core/diff
  (:use #:cl)
  (:export #:comparison-form-p
           #:format-value-diff
           #:maybe-value-diff))
(in-package #:rove/core/diff)

(defparameter *comparison-ops*
  '(equal equalp string= string-equal eql =))

(defun comparison-form-p (form)
  (and (consp form)
       (symbolp (first form))
       (member (first form) *comparison-ops* :test #'eq)
       (= (length (rest form)) 2)))

(defun split-lines (string)
  (let ((s (if (stringp string) string (prin1-to-string string)))
        (lines '())
        (start 0))
    (loop for i from 0 below (length s)
          when (char= (char s i) #\Newline)
            do (push (subseq s start i) lines)
               (setf start (1+ i)))
    (push (subseq s start) lines)
    (nreverse lines)))

(defun format-value-diff (expected actual)
  (with-output-to-string (out)
    (format out "~%    expected: ~S~%    actual:   ~S" expected actual)
    (when (and (stringp expected) (stringp actual)
               (find #\Newline expected) (find #\Newline actual))
      (format out "~%    --- expected~%    +++ actual")
      (let ((e-lines (split-lines expected))
            (a-lines (split-lines actual)))
        (loop for e in e-lines
              for a in a-lines
              do (cond
                   ((equal e a)
                    (format out "~%     ~A" e))
                   (t
                    (format out "~%    -~A~%    +~A" e a)))
              finally
                 (let ((n-e (length e-lines))
                       (n-a (length a-lines)))
                   (cond
                     ((> n-e n-a)
                      (dolist (e (nthcdr n-a e-lines))
                        (format out "~%    -~A" e)))
                     ((> n-a n-e)
                      (dolist (a (nthcdr n-e a-lines))
                        (format out "~%    +~A" a))))))))))

(defun maybe-value-diff (form values)
  (when (and (comparison-form-p form)
             (listp values)
             (= (length values) 2))
    (format-value-diff (first values) (second values))))
