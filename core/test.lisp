(in-package #:cl-user)
(defpackage #:rove/core/test
  (:use #:cl
        #:rove/core/stats)
  (:import-from #:rove/core/assertion
                #:*debug-on-error*
                #:failed-assertion)
  (:import-from #:rove/core/suite/package
                #:*execute-assertions*
                #:wrap-if-toplevel
                #:package-suite
                #:suite-tests)
  (:import-from #:dissect
                #:stack)
  (:export #:deftest
           #:testing
           #:package-tests
           #:run-test
           #:run-package-tests))
(in-package #:rove/core/test)

(defmacro deftest (name &body body)
  (let ((test-name (let ((*print-case* :downcase))
                     (princ-to-string name))))
    `(progn
       (pushnew ',name (suite-tests (package-suite *package*))
                :test 'eq)

       (defun ,name ()
         (testing ,test-name
           ,@body)))))

(defmacro testing (desc &body body)
  (let ((main (gensym "MAIN")))
    `(wrap-if-toplevel
       (test-begin *stats* ,desc)
       (unwind-protect
            (flet ((,main () ,@body))
              (if *debug-on-error*
                  (,main)
                  (block nil
                    (handler-bind ((error
                                     (lambda (e)
                                       (record *stats*
                                               (make-instance 'failed-assertion
                                                              :form t
                                                              :reason e
                                                              :stacks (dissect:stack)
                                                              :desc "Raise an error while testing."))
                                       (return nil))))
                      (,main)))))
         (test-finish *stats* ,desc)))))

(defun package-tests (package)
  (reverse (suite-tests (package-suite package))))

(defun run-package-tests (package)
  (check-type package package)
  (let ((test-name (string-downcase (package-name package)))
        (tests (package-tests package))
        (*execute-assertions* t)
        (*package* package))
    (test-begin *stats* test-name (length tests))
    (unwind-protect (dolist (test tests)
                      (funcall test))
      (test-finish *stats* test-name))))
