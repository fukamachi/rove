(in-package #:cl-user)
(defpackage #:rove/core/diff
  (:use #:cl)
  (:export #:*diff-print-length*
           #:*diff-print-level*
           #:comparison-form-p
           #:format-value-diff
           #:maybe-value-diff))
(in-package #:rove/core/diff)

(defvar *diff-print-length* 100
  "*PRINT-LENGTH* used while printing expected/actual values, unless already bound.")
(defvar *diff-print-level* 10
  "*PRINT-LEVEL* used while printing expected/actual values, unless already bound.")

(defparameter *comparison-ops*
  '(equal equalp string= string-equal eql =))

(defun comparison-form-p (form)
  (and (consp form)
       (symbolp (first form))
       (member (first form) *comparison-ops* :test #'eq)
       (= (length (rest form)) 2)))

(defun split-lines (string)
  (let ((lines '())
        (start 0))
    (loop for i from 0 below (length string)
          when (char= (char string i) #\Newline)
            do (push (subseq string start i) lines)
               (setf start (1+ i)))
    (push (subseq string start) lines)
    (nreverse lines)))

(defun line-diff (expected actual)
  "Longest-common-subsequence diff of two line lists.
Returns a list of (:same line), (:delete line) and (:insert line)."
  (let* ((n (length expected))
         (m (length actual))
         (e (coerce expected 'vector))
         (a (coerce actual 'vector))
         ;; lcs[i][j] = LCS length of e[i..] and a[j..]
         (lcs (make-array (list (1+ n) (1+ m)) :element-type 'fixnum :initial-element 0)))
    (loop for i from (1- n) downto 0
          do (loop for j from (1- m) downto 0
                   do (setf (aref lcs i j)
                            (if (string= (aref e i) (aref a j))
                                (1+ (aref lcs (1+ i) (1+ j)))
                                (max (aref lcs (1+ i) j)
                                     (aref lcs i (1+ j)))))))
    (let ((i 0) (j 0) (ops '()))
      (loop while (or (< i n) (< j m))
            do (cond
                 ((and (< i n) (< j m) (string= (aref e i) (aref a j)))
                  (push (list :same (aref e i)) ops)
                  (incf i) (incf j))
                 ((and (< j m)
                       (or (= i n) (>= (aref lcs i (1+ j)) (aref lcs (1+ i) j))))
                  (push (list :insert (aref a j)) ops)
                  (incf j))
                 (t
                  (push (list :delete (aref e i)) ops)
                  (incf i))))
      (nreverse ops))))

(defun format-value-diff (expected actual)
  (let ((*print-length* (or *print-length* *diff-print-length*))
        (*print-level* (or *print-level* *diff-print-level*)))
    (with-output-to-string (out)
      (format out "~%    expected: ~S~%    actual:   ~S" expected actual)
      (when (and (stringp expected) (stringp actual)
                 (find #\Newline expected) (find #\Newline actual))
        (format out "~%    --- expected~%    +++ actual")
        (dolist (op (line-diff (split-lines expected) (split-lines actual)))
          (destructuring-bind (kind line) op
            (format out "~%    ~A~A"
                    (ecase kind (:same " ") (:delete "-") (:insert "+"))
                    line)))))))

(defun maybe-value-diff (form values)
  "Return a diff string when FORM is a two-argument comparison and VALUES
are its two argument values, otherwise NIL."
  (when (and (comparison-form-p form)
             (listp values)
             (= (length values) 2))
    (format-value-diff (first values) (second values))))
