(in-package #:cl-user)
(defpackage #:rove/core/test
  (:use #:cl
        #:rove/core/stats)
  (:import-from #:rove/core/assertion
                #:*debug-on-error*
                #:failed-assertion)
  (:import-from #:rove/core/suite/package
                #:*package-suites*
                #:*execute-assertions*
                #:wrap-if-toplevel)
  (:import-from #:dissect
                #:stack)
  (:export #:deftest
           #:testing
           #:package-tests
           #:clear-package-tests
           #:run-test
           #:run-package-tests))
(in-package #:rove/core/test)

(defmacro deftest (name &body body)
  (let ((test-name (let ((*print-case* :downcase))
                     (princ-to-string name))))
    `(progn
       (pushnew ',name (gethash *package* *package-suites*)
                :test 'eq)

       (defun ,name ()
         (testing ,test-name
           ,@body)))))

(defmacro testing (desc &body body)
  `(wrap-if-toplevel
     (test-begin *stats* ,desc)
     (unwind-protect
          (if *debug-on-error*
              (progn ,@body)
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
                  ,@body)))
       (test-finish *stats* ,desc))))

(defun package-tests (package)
  (reverse (gethash package *package-suites*)))

(defun clear-package-tests (package)
  (check-type package package)
  (remhash package *package-suites*))

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
