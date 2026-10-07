(in-package #:cl-user)
(defpackage #:rove/core/marks
  (:use #:cl)
  (:export #:*mark-expr*
           #:normalize-mark
           #:normalize-mark-expr
           #:test-marks
           #:set-test-marks
           #:eval-mark-expr
           #:test-selected-by-marks-p))
(in-package #:rove/core/marks)

(defvar *mark-expr* nil
  "When non-NIL, only tests whose marks satisfy this expression run.
   A keyword, or (AND …) / (OR …) / (NOT …) of the same.")

(defun normalize-mark (mark)
  (cond
    ((keywordp mark) mark)
    ((and mark (symbolp mark)) (intern (symbol-name mark) :keyword))
    ((stringp mark) (intern (string-upcase mark) :keyword))
    (t (error "Invalid test mark: ~S" mark))))

(defun test-marks (name)
  (check-type name symbol)
  (get name 'rove-marks))

(defun set-test-marks (name marks)
  (check-type name symbol)
  (setf (get name 'rove-marks)
        (mapcar #'normalize-mark (if (listp marks) marks (list marks))))
  (test-marks name))

(defun normalize-mark-expr (expr)
  "Validate EXPR and return it with marks as keywords and operators as AND / OR / NOT.
   NIL and T both mean \"select every test\"."
  (labels ((walk (expr)
             (cond
               ((or (symbolp expr) (stringp expr))
                (normalize-mark expr))
               ((and (consp expr) (symbolp (first expr)) (first expr))
                (let ((op (find (first expr) '(and or not) :test #'string=))
                      (args (rest expr)))
                  (unless (and op args (or (not (eq op 'not)) (null (rest args))))
                    (error "Invalid mark expression: ~S" expr))
                  (cons op (mapcar #'walk args))))
               (t
                (error "Invalid mark expression: ~S" expr)))))
    (if (member expr '(nil t))
        t
        (walk expr))))

(defun eval-mark-expr (expr marks)
  (let ((expr (normalize-mark-expr expr)))
    (labels ((eval-expr (expr)
               (cond
                 ((eq expr t) t)
                 ((keywordp expr) (and (member expr marks :test #'eq) t))
                 (t
                  (ecase (first expr)
                    (and (every #'eval-expr (rest expr)))
                    (or (some #'eval-expr (rest expr)))
                    (not (not (eval-expr (second expr)))))))))
      (eval-expr expr))))

(defun test-selected-by-marks-p (name)
  (eval-mark-expr *mark-expr* (test-marks name)))
