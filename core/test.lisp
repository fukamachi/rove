(in-package #:cl-user)
(defpackage #:rove/core/test
  (:use #:cl
        #:rove/core/stats
        #:rove/core/suite/package)
  (:import-from #:rove/core/assertion
                #:*debug-on-error*
                #:failed-assertion)
  (:import-from #:dissect
                #:stack)
  (:export #:deftest
           #:testing
           #:setup
           #:teardown
           #:defhook
           #:package-tests
           #:run-test
           #:run-package-tests))
(in-package #:rove/core/test)

(defmacro deftest (name &body body)
  (let ((test-name (let ((*print-case* :downcase))
                     (princ-to-string name))))
    `(set-test ',name (lambda ()
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
                                                              :labels (and *stats*
                                                                           (stats-context-labels *stats*))
                                                              :desc "Raise an error while testing."))
                                       (return nil))))
                      (,main)))))
         (test-finish *stats* ,desc)))))

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

(defmacro defhook (mode &body body)
  (let ((main (gensym "MAIN")))
    `(flet ((,main () ,@body))
       (pushnew #',main
                ,(ecase mode
                   (:before `(suite-before-hooks (package-suite *package*)))
                   (:after `(suite-after-hooks (package-suite *package*)))))
       (values))))

(defun package-tests (package)
  (reverse (suite-tests (package-suite package))))

(defun run-package-tests (package)
  (check-type package package)
  (let ((test-name (string-downcase (package-name package)))
        (suite (package-suite package))
        (*execute-assertions* t)
        (*package* package))
    (test-begin *stats* test-name (length (suite-tests suite)))
    (unwind-protect (run-suite suite)
      (test-finish *stats* test-name))))
