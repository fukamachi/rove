(in-package #:cl-user)
(defpackage #:rove/core/parametrize
  (:use #:cl)
  (:import-from #:rove/core/test
                #:deftest
                #:*default-test-compilation-time*
                #:call-with-testing-with-options)
  (:import-from #:rove/core/suite/package
                #:set-test)
  (:export #:deftest-parametrize
           #:parametrize-test-name))
(in-package #:rove/core/parametrize)

(defun parametrize-test-name (base index &optional id)
  "Intern a per-row test symbol: BASE/INDEX or BASE/ID."
  (check-type base symbol)
  (let* ((pkg (or (symbol-package base) *package*))
         (suffix (if id
                     (string-upcase (princ-to-string id))
                     (princ-to-string index)))
         (name (format nil "~A/~A" (symbol-name base) suffix)))
    (intern name pkg)))

(defun %normalize-vars (vars-form)
  (cond
    ((and (symbolp vars-form) (not (null vars-form)))
     (list vars-form))
    ((and (consp vars-form) (every #'symbolp vars-form))
     vars-form)
    (t (error "deftest-parametrize: vars must be a symbol or list of symbols, got ~S"
              vars-form))))

(defun %normalize-row (vars row)
  "Coerce ROW to a list matching VARS length."
  (let ((vals (cond
                ((= (length vars) 1)
                 (cond
                   ((and (consp row) (null (cdr row))) row)
                   ((and (consp row) (listp (cdr row)) (cdr row))
                    (error "deftest-parametrize: row ~S has ~D values, expected 1"
                           row (length row)))
                   (t (list row))))
                ((and (consp row) (listp (cdr row)))
                 row)
                (t (error "deftest-parametrize: row ~S is not a list" row)))))
    (unless (= (length vals) (length vars))
      (error "deftest-parametrize: row ~S has ~D values, expected ~D for ~S"
             row (length vals) (length vars) vars))
    vals))

(defun %parse-bindings (bindings)
  "Return (values vars ids-form rows).
   ROWS is a literal list, or (:eval form) when :rows was supplied."
  (unless (consp bindings)
    (error "deftest-parametrize: bindings must be a list, got ~S" bindings))
  (let* ((vars (%normalize-vars (first bindings)))
         (rest (cdr bindings))
         (ids-form nil)
         (rows nil))
    (loop while (and rest (keywordp (first rest)))
          do (ecase (first rest)
               (:ids
                (setf ids-form (second rest)
                      rest (cddr rest)))
               (:rows
                (setf rows (list :eval (second rest))
                      rest (cddr rest)))))
    (when (and (consp rows) (eq (first rows) :eval) rest)
      (error "deftest-parametrize: cannot mix :rows with literal rows"))
    (unless rows
      (setf rows rest))
    (values vars ids-form rows)))

(defun %ids-list (ids-form)
  (cond
    ((null ids-form) nil)
    ((and (consp ids-form) (eq (first ids-form) 'quote))
     (second ids-form))
    ((listp ids-form) ids-form)
    (t (error "deftest-parametrize: :ids must be a list, got ~S" ids-form))))

(defmacro deftest-parametrize (name-and-options bindings &body body)
  "Define one Rove test per parameter row (pytest.mark.parametrize shape).

   BINDINGS:

     ((var*) [:ids ids] [:rows form] row*)

   - VAR* — symbols bound in BODY for each row
   - ROW  — list of values (or a bare value when there is one var)
   - :ids — list of suffixes (string/symbol/number) for test names;
            default is 0-based indices → NAME/0, NAME/1, …
   - :rows — form evaluated at load/execute time → list of rows

   NAME-AND-OPTIONS — same as DEFTEST (`name` or `(name :compile-at …)`).
   For `:compile-at :run-time`, each expanded DEFTEST inherits the option.

   Examples:

     (deftest-parametrize add
         ((a b expected)
          (1 2 3)
          (0 0 0))
       (ok (= (+ a b) expected)))

     (deftest-parametrize parity
         ((n) :ids (\"even\" \"odd\")
          2 3)
       (ok (evenp n)))"
  (destructuring-bind (base &rest deftest-options &key (compile-at *default-test-compilation-time*)
                       &allow-other-keys)
      (if (consp name-and-options)
          name-and-options
          (list name-and-options))
    (declare (ignore compile-at))
    (multiple-value-bind (vars ids-form rows)
        (%parse-bindings bindings)
      (if (and (consp rows) (eq (first rows) :eval))
          (let ((rows-g (gensym "ROWS"))
                (ids-g (gensym "IDS"))
                (i-g (gensym "I"))
                (row-g (gensym "ROW"))
                (vals-g (gensym "VALS"))
                (sym-g (gensym "SYM"))
                (id-g (gensym "ID"))
                (desc-g (gensym "DESC")))
            `(eval-when (:load-toplevel :execute)
               (let ((,rows-g ,(second rows))
                     (,ids-g ,ids-form))
                 (when (and ,ids-g (/= (length ,ids-g) (length ,rows-g)))
                   (error "deftest-parametrize: :ids length ~D != rows ~D"
                          (length ,ids-g) (length ,rows-g)))
                 (loop for ,row-g in ,rows-g
                       for ,i-g from 0
                       for ,id-g = (when ,ids-g (nth ,i-g ,ids-g))
                       for ,vals-g = (%normalize-row ',vars ,row-g)
                       for ,sym-g = (parametrize-test-name ',base ,i-g ,id-g)
                       for ,desc-g = (let ((*print-case* :downcase))
                                       (princ-to-string ,sym-g))
                       do (set-test
                           ,sym-g
                           (let ((vals ,vals-g)
                                 (name ,sym-g)
                                 (desc ,desc-g)
                                 (body-fn (compile nil '(lambda (,@vars) ,@body))))
                             (lambda ()
                               (call-with-testing-with-options
                                desc name
                                (lambda ()
                                  (apply body-fn vals)))))))
                 ',base)))
          (let ((ids (%ids-list ids-form)))
            (when (and ids (/= (length ids) (length rows)))
              (error "deftest-parametrize: :ids length ~D != rows ~D"
                     (length ids) (length rows)))
            `(progn
               ,@(loop for row in rows
                       for i from 0
                       for id = (when ids (nth i ids))
                       for vals = (%normalize-row vars row)
                       for test-sym = (parametrize-test-name base i id)
                       for name-form = (if deftest-options
                                           (list* test-sym deftest-options)
                                           test-sym)
                       collect
                       `(deftest ,name-form
                          (let ,(mapcar #'list vars vals)
                            ,@body)))
               ',base))))))
