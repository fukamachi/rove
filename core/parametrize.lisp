(in-package #:cl-user)
(defpackage #:rove/core/parametrize
  (:use #:cl)
  (:import-from #:rove/core/test
                #:deftest)
  (:export #:deftest-parametrize
           #:parametrize-test-name))
(in-package #:rove/core/parametrize)

(defun parametrize-test-name (base index &optional id)
  "Return a symbol for a parametrized row: BASE/INDEX or BASE/ID."
  (check-type base symbol)
  (let* ((package (or (symbol-package base) *package*))
         (suffix (if id
                     (string-upcase (princ-to-string id))
                     (princ-to-string index))))
    (intern (format nil "~A/~A" (symbol-name base) suffix) package)))

(defun normalize-vars (vars-form)
  (cond
    ((and (symbolp vars-form) vars-form)
     (list vars-form))
    ((and (consp vars-form) (every #'symbolp vars-form))
     vars-form)
    (t
     (error "DEFTEST-PARAMETRIZE: vars must be a symbol or list of symbols, got ~S"
            vars-form))))

(defun normalize-row (vars row)
  "Coerce ROW to a value list matching VARS."
  (let ((values
          (cond
            ((= (length vars) 1)
             (cond
               ((and (consp row) (null (cdr row)))
                row)
               ((and (consp row) (listp (cdr row)) (cdr row))
                (error "DEFTEST-PARAMETRIZE: row ~S has ~D values, expected 1"
                       row (length row)))
               (t (list row))))
            ((and (consp row) (listp (cdr row)))
             row)
            (t
             (error "DEFTEST-PARAMETRIZE: row ~S is not a list" row)))))
    (unless (= (length values) (length vars))
      (error "DEFTEST-PARAMETRIZE: row ~S has ~D values, expected ~D for ~S"
             row (length values) (length vars) vars))
    values))

(defun parse-bindings (bindings)
  "Parse BINDINGS → (values vars ids-form rows).
   ROWS is either a list of literal rows, or (:rows FORM)."
  (unless (consp bindings)
    (error "DEFTEST-PARAMETRIZE: bindings must be a list, got ~S" bindings))
  (let ((vars (normalize-vars (first bindings)))
        (rest (rest bindings))
        (ids-form nil)
        (rows nil))
    (loop while (and rest (keywordp (first rest)))
          do (ecase (first rest)
               (:ids
                (setf ids-form (second rest)
                      rest (cddr rest)))
               (:rows
                (setf rows (list :rows (second rest))
                      rest (cddr rest)))))
    (when (and (consp rows) (eq (first rows) :rows) rest)
      (error "DEFTEST-PARAMETRIZE: cannot mix :rows with literal rows"))
    (unless rows
      (setf rows rest))
    (when (and (listp rows)
               (not (and (consp rows) (eq (first rows) :rows)))
               (null rows))
      (error "DEFTEST-PARAMETRIZE: no parameter rows given"))
    (values vars ids-form rows)))

(defun ids-list (ids-form)
  (cond
    ((null ids-form) nil)
    ((and (consp ids-form) (eq (first ids-form) 'quote))
     (second ids-form))
    ((listp ids-form) ids-form)
    (t
     (error "DEFTEST-PARAMETRIZE: :ids must be a list, got ~S" ids-form))))

(defun expand-parametrized-deftest (name-form vars values body)
  `(deftest ,name-form
     (let ,(mapcar #'list vars values)
       ,@body)))

(defmacro deftest-parametrize (name-and-options bindings &body body)
  "Define one test per parameter row.

Syntax:

  (deftest-parametrize name-and-options
      ((var*) [:ids ids] [:rows form] row*)
    body*)

NAME-AND-OPTIONS is the same as DEFTEST: NAME or (NAME :compile-at …).

Each ROW is a list of values for VAR*, or a single value when there is
one variable. Tests are registered as NAME/0, NAME/1, … unless :ids
supplies per-row suffixes (NAME/ID).

:rows FORM evaluates FORM at load/execute time; the value must be a
list of rows. This is for tables that are not literal constants.

Examples:

  (deftest-parametrize add
      ((a b expected)
       (1 2 3)
       (0 0 0))
    (ok (= (+ a b) expected)))

  (deftest-parametrize doubles
      ((n expected) :ids (\"two\" \"four\")
       (1 2)
       (2 4))
    (ok (= (* n 2) expected)))"
  (destructuring-bind (base &rest deftest-options)
      (if (consp name-and-options)
          name-and-options
          (list name-and-options))
    (multiple-value-bind (vars ids-form rows)
        (parse-bindings bindings)
      (if (and (consp rows) (eq (first rows) :rows))
          ;; Dynamic table — register via DEFTEST so compile-at / hooks stay shared.
          (let ((rows-var (gensym "ROWS"))
                (ids-var (gensym "IDS"))
                (index-var (gensym "INDEX"))
                (row-var (gensym "ROW"))
                (values-var (gensym "VALUES"))
                (test-var (gensym "TEST"))
                (id-var (gensym "ID"))
                (name-form-var (gensym "NAME-FORM")))
            `(eval-when (:load-toplevel :execute)
               (let ((,rows-var ,(second rows))
                     (,ids-var ,ids-form))
                 (unless (listp ,rows-var)
                   (error "DEFTEST-PARAMETRIZE: :rows must evaluate to a list, got ~S"
                          ,rows-var))
                 (when (and ,ids-var (/= (length ,ids-var) (length ,rows-var)))
                   (error "DEFTEST-PARAMETRIZE: :ids length ~D does not match ~D rows"
                          (length ,ids-var) (length ,rows-var)))
                 (loop for ,row-var in ,rows-var
                       for ,index-var from 0
                       for ,id-var = (when ,ids-var (nth ,index-var ,ids-var))
                       for ,values-var = (normalize-row ',vars ,row-var)
                       for ,test-var = (parametrize-test-name ',base ,index-var ,id-var)
                       for ,name-form-var = (if ',deftest-options
                                                (list* ,test-var ',deftest-options)
                                                ,test-var)
                       do (eval (expand-parametrized-deftest
                                 ,name-form-var ',vars ,values-var ',body)))
                 ',base)))
          (let ((ids (ids-list ids-form)))
            (when (and ids (/= (length ids) (length rows)))
              (error "DEFTEST-PARAMETRIZE: :ids length ~D does not match ~D rows"
                     (length ids) (length rows)))
            `(progn
               ,@(loop for row in rows
                       for index from 0
                       for id = (when ids (nth index ids))
                       for values = (normalize-row vars row)
                       for test-symbol = (parametrize-test-name base index id)
                       for name-form = (if deftest-options
                                           (list* test-symbol deftest-options)
                                           test-symbol)
                       collect (expand-parametrized-deftest name-form vars values body))
               ',base))))))
