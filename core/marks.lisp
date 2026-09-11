(in-package #:cl-user)
(defpackage #:rove/core/marks
  (:use #:cl)
  (:export #:*mark-expr*
           #:normalize-mark
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
    ((symbolp mark) (intern (symbol-name mark) :keyword))
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

(defun eval-mark-expr (expr marks)
  (cond
    ((null expr) t)
    ((eq expr t) t)
    ((keywordp expr)
     (not (null (member expr marks :test #'eq))))
    ((symbolp expr)
     (eval-mark-expr (normalize-mark expr) marks))
    ((stringp expr)
     (eval-mark-expr (normalize-mark expr) marks))
    ((consp expr)
     (let ((op (first expr)))
       (cond
         ((member op '(or :or) :test #'eq)
          (some (lambda (e) (eval-mark-expr e marks)) (rest expr)))
         ((member op '(and :and) :test #'eq)
          (every (lambda (e) (eval-mark-expr e marks)) (rest expr)))
         ((member op '(not :not) :test #'eq)
          (not (eval-mark-expr (second expr) marks)))
         (t
          (error "Invalid mark expression: ~S" expr)))))
    (t
     (error "Invalid mark expression: ~S" expr))))

(defun test-selected-by-marks-p (name)
  (eval-mark-expr *mark-expr* (test-marks name)))
