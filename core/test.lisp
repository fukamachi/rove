(in-package #:cl-user)
(defpackage #:rove/core/test
  (:use #:cl
        #:rove/core/stats
        #:rove/core/suite/package)
  (:import-from #:rove/core/assertion
                #:*debug-on-error*
                #:*quit-on-failure*
                #:failed-assertion)
  (:import-from #:dissect
                #:stack)
  (:export #:deftest
           #:testing
           #:setup
           #:teardown
           #:defhook
           #:*default-test-compilation-time*))
(in-package #:rove/core/test)

(defvar *default-test-compilation-time* :definition-time)

(defun call-with-testing-with-options (desc name function)
  (test-begin *stats* desc)
  (unwind-protect
       (with-context (context :name name :description desc)
         (if *debug-on-error*
             (funcall function)
             (block nil
               (handler-bind ((error
                                (lambda (e)
                                  (record *stats*
                                          (make-instance 'failed-assertion
                                                         :form t
                                                         :reason e
                                                         :stacks (dissect:stack)
                                                         :labels (and *stats*
                                                                      (stats-context-labels *stats*))
                                                         :desc "Raise an error while testing."))
                                  (return nil))))
                 (funcall function)))))
    (when (and *quit-on-failure*
               (not (passedp (stats-context *stats*))))
      (format t "Failed test, with the abort option enabled. ~%")
      (abort))

    (test-finish *stats* desc)))

(defmacro with-testing-with-options (desc (&key name) &body body)
  `(call-with-testing-with-options ,desc ,name (lambda () ,@body)))

(defmacro deftest (name-and-options &body body)
  (destructuring-bind (name &key (compile-at *default-test-compilation-time*))
      (if (consp name-and-options)
          name-and-options
          (list name-and-options))
    (check-type compile-at (member :run-time :definition-time))
    (let ((test-name (let ((*print-case* :downcase))
                       (princ-to-string name))))
      `(set-test ',name
                 ,(if (eq compile-at :run-time)
                    `(lambda ()
                       (funcall (compile nil '(lambda ()
                                                (with-testing-with-options ,test-name (:name ',name)
                                                  ,@body)))))
                    `(lambda ()
                       (with-testing-with-options ,test-name (:name ',name) ,@body)))))))

(defmacro testing (desc &body body)
  `(with-testing-with-options ,desc () ,@body))

(defmacro setup (&body body)
  `(progn
     (setf (suite-setup (package-suite *package*))
           (lambda () ,@body))
     (values)))

(defmacro teardown (&body body)
  `(progn
     (setf (suite-teardown (package-suite *package*))
           (lambda () ,@body))
     (values)))

(defmacro defhook (name &optional mode &body body)
  (let ((main (gensym "MAIN"))
        (existing-hook (gensym "EXISTING-HOOK"))
        (no-name-hook (member name '(:before :after) :test 'eq)))
    (destructuring-bind (name mode &rest body)
        (if no-name-hook
            (list* nil name mode body)
            (list* name mode body))
      `(flet ((,main ()
                ,@body))
         (let ((,existing-hook (assoc ',name
                                      ,(ecase mode
                                         (:before `(suite-before-hooks (package-suite *package*)))
                                         (:after `(suite-after-hooks (package-suite *package*)))))))
           (if ,existing-hook
               (setf (cdr ,existing-hook) #',main)
               (push (cons ',name #',main)
                     ,(ecase mode
                        (:before `(suite-before-hooks (package-suite *package*)))
                        (:after `(suite-after-hooks (package-suite *package*)))))))
         (values)))))
