(in-package #:cl-user)
(defpackage #:rove/core/fixture
  (:use #:cl)
  (:export #:deffixture
           #:with-fixture
           #:with-fixtures
           #:yield))
(in-package #:rove/core/fixture)

(deftype fixture-scope () '(member :test :suite :session))

(defvar *fixtures* (make-hash-table :test 'eq))

;; Cached :suite / :session fixtures as (name value teardown), newest first.
;; Rebound per suite / per run so nested runs cannot tear each other down.
(defvar *suite-fixture-cache* '())
(defvar *session-fixture-cache* '())

(defclass fixture ()
  ((name :initarg :name :reader fixture-name)
   (scope :initarg :scope :reader fixture-scope)
   (function :initarg :function :reader fixture-function
             :documentation "Called with a continuation taking (value teardown-thunk).")))

(defmacro yield (&optional value)
  (declare (ignore value))
  (error "YIELD is only valid as a top-level form of a DEFFIXTURE body."))

(defun find-fixture (name)
  (or (gethash name *fixtures*)
      (error "No fixture named ~S" name)))

(defun cache-for (scope)
  (ecase scope
    (:suite '*suite-fixture-cache*)
    (:session '*session-fixture-cache*)))

(defun run-teardowns (entries)
  "Run the teardown of every cached ENTRY, newest first, even if one signals."
  (when entries
    (unwind-protect (funcall (third (first entries)))
      (run-teardowns (rest entries)))))

(defun teardown-fixtures (scope)
  "Tear down and forget every cached fixture of SCOPE (:suite or :session)."
  (let* ((cache (cache-for scope))
         (entries (symbol-value cache)))
    (setf (symbol-value cache) '())
    (run-teardowns entries)))

(defun call-with-fixture-session (function)
  "Run FUNCTION with fresh :suite / :session caches and tear them down afterwards."
  (let ((*suite-fixture-cache* '())
        (*session-fixture-cache* '()))
    (unwind-protect (funcall function)
      (unwind-protect (teardown-fixtures :session)
        (teardown-fixtures :suite)))))

(defmacro with-fixture-session (() &body body)
  `(call-with-fixture-session (lambda () ,@body)))

(defun evict-fixture (name)
  "Drop NAME from both caches, running its teardown, so a redefinition takes effect."
  (dolist (cache '(*suite-fixture-cache* *session-fixture-cache*))
    (let ((entry (assoc name (symbol-value cache))))
      (when entry
        (setf (symbol-value cache) (remove entry (symbol-value cache)))
        (funcall (third entry))))))

(defun set-fixture (name scope function)
  (check-type name symbol)
  (check-type scope fixture-scope)
  (evict-fixture name)
  (setf (gethash name *fixtures*)
        (make-instance 'fixture :name name :scope scope :function function))
  name)

(defun call-with-fixture (name body)
  (let* ((fixture (find-fixture name))
         (scope (fixture-scope fixture)))
    (if (eq scope :test)
        (funcall (fixture-function fixture)
                 (lambda (value teardown)
                   (unwind-protect (funcall body value)
                     (funcall teardown))))
        (let* ((cache (cache-for scope))
               (entry (assoc name (symbol-value cache))))
          (if entry
              (funcall body (second entry))
              (funcall (fixture-function fixture)
                       (lambda (value teardown)
                         (push (list name value teardown) (symbol-value cache))
                         (funcall body value))))))))

;;; DEFFIXTURE rewrites its body at macroexpansion time: everything after
;;; (yield value) becomes a teardown closure, so it keeps the fixture's lexical
;;; bindings and can run under UNWIND-PROTECT or at suite / session end.

(defun yield-form-p (form)
  (and (consp form) (eq (first form) 'yield)))

(defun contains-yield-p (form)
  (cond
    ((yield-form-p form) t)
    ((consp form) (or (contains-yield-p (car form))
                      (contains-yield-p (cdr form))))
    (t nil)))

(defparameter *yield-wrappers* '(let let* progn locally)
  "Forms DEFFIXTURE looks inside to find YIELD.")

(defun split-at-yield (forms continue)
  "Rewrite FORMS so that (yield VALUE) becomes (funcall CONTINUE VALUE teardown-thunk)."
  (let ((position (position-if #'contains-yield-p forms)))
    (unless position
      (error "DEFFIXTURE body must call YIELD."))
    (let ((before (subseq forms 0 position))
          (form (nth position forms))
          (after (nthcdr (1+ position) forms)))
      (when (some #'contains-yield-p after)
        (error "DEFFIXTURE body may call YIELD only once."))
      (cond
        ((yield-form-p form)
         (unless (<= (length form) 2)
           (error "YIELD takes at most one value: ~S" form))
         `(,@before (funcall ,continue ,(second form) (lambda () ,@after))))
        ((and (consp form) (member (first form) *yield-wrappers*))
         (when after
           (error "Forms after the ~S containing YIELD must move after YIELD: ~S"
                  (first form) after))
         (destructuring-bind (operator &rest rest) form
           (if (member operator '(let let*))
               `(,@before (,operator ,(first rest) ,@(split-at-yield (rest rest) continue)))
               `(,@before (,operator ,@(split-at-yield rest continue))))))
        (t
         (error "YIELD must be a top-level form of the DEFFIXTURE body (or inside ~{~S~^ / ~}), found it inside ~S"
                *yield-wrappers* form))))))

(defmacro deffixture (name (&key (scope :test)) &body body)
  "Define a fixture. The forms before (yield VALUE) are setup, VALUE is what
WITH-FIXTURE binds, and the forms after YIELD are teardown.

SCOPE is :test (setup and teardown around every WITH-FIXTURE), :suite (once
per package suite, torn down when the suite finishes) or :session (once per
RUN, torn down when it finishes). Teardown also runs when the test body
exits non-locally (error, timeout, throw).

YIELD must appear exactly once, as a top-level form of the body or inside
LET / LET* / PROGN / LOCALLY."
  (check-type name symbol)
  (check-type scope fixture-scope)
  (let ((continue (gensym "CONTINUE")))
    `(set-fixture ',name ,scope
                  (lambda (,continue)
                    ,@(split-at-yield body continue)))))

(defmacro with-fixture ((var name) &body body)
  `(call-with-fixture ',name (lambda (,var) ,@body)))

(defmacro with-fixtures (bindings &body body)
  (if (null bindings)
      `(progn ,@body)
      (destructuring-bind (binding . rest) bindings
        (unless (and (consp binding) (= (length binding) 2) (symbolp (first binding)))
          (error "WITH-FIXTURES binding must be (VAR NAME), got ~S" binding))
        `(with-fixture (,(first binding) ,(second binding))
           (with-fixtures ,rest
             ,@body)))))
