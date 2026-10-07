(in-package #:cl-user)
(defpackage #:rove/core/test
  (:use #:cl
        #:rove/core/stats
        #:rove/core/suite/package)
  (:import-from #:rove/core/assertion
                #:*debug-on-error*
                #:*quit-on-failure*
                #:failed-assertion
                #:quit-early)
  (:import-from #:rove/core/result
                #:passed-assertion
                #:assertion-description
                #:*suppress-assertion-printing*)
  (:import-from #:dissect
                #:stack)
  (:import-from #:rove/core/marks
                #:set-test-marks)
  (:import-from #:bordeaux-threads)
  (:export #:deftest
           #:testing
           #:failing
           #:setup
           #:teardown
           #:defhook
           #:*default-test-compilation-time*
           #:*default-test-timeout*))
(in-package #:rove/core/test)

(defvar *default-test-compilation-time* :definition-time)

(deftype timeout-seconds () '(or null (real (0))))
(declaim (type timeout-seconds *default-test-timeout*))
(defvar *default-test-timeout* nil
  "Default per-test timeout in seconds. NIL means no timeout.")

(defun test-timeout (name)
  (or (get name 'rove-timeout) *default-test-timeout*))

(defun set-test-timeout (name seconds)
  (check-type seconds timeout-seconds)
  (setf (get name 'rove-timeout) seconds))

(defun call-with-test-timeout (seconds thunk)
  (if seconds
      (handler-case
          (bt2:with-timeout (seconds)
            (funcall thunk))
        (bt2:timeout ()
          (record *stats*
                  (make-instance 'failed-assertion
                                 :form `(timeout ,seconds)
                                 :labels (and *stats*
                                              (stats-context-labels *stats*))
                                 :desc (format nil "Test exceeded timeout of ~A second~:P"
                                               seconds)))
          nil))
      (funcall thunk)))

(defun call-with-testing-with-options (desc name function &optional timeout)
  (test-begin *stats* desc)
  (unwind-protect
       (with-context (context :name (or name desc) :description desc)
         (if *debug-on-error*
             (call-with-test-timeout timeout function)
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
                 (call-with-test-timeout timeout function)))))
    (test-finish *stats* desc)

    (when (and *quit-on-failure*
               (not (passedp (stats-context *stats*))))
      (error 'quit-early))))

(defmacro with-testing-with-options (desc (&key name timeout) &body body)
  `(call-with-testing-with-options ,desc ,name (lambda () ,@body) ,timeout))

(defmacro deftest (name-and-options &body body)
  (destructuring-bind (name &key (compile-at *default-test-compilation-time*)
                                 marks
                                 timeout)
      (if (consp name-and-options)
          name-and-options
          (list name-and-options))
    (check-type compile-at (member :run-time :definition-time))
    (let ((test-name (let ((*print-case* :downcase))
                       (princ-to-string name))))
      `(progn
         (set-test-marks ',name ',marks)
         (set-test-timeout ',name ,timeout)
         (set-test ',name
                   ,(if (eq compile-at :run-time)
                      `(lambda ()
                         (funcall (compile nil '(lambda ()
                                                  (with-testing-with-options ,test-name
                                                      (:name ',name :timeout (test-timeout ',name))
                                                    ,@body)))))
                      `(lambda ()
                         (with-testing-with-options ,test-name
                             (:name ',name :timeout (test-timeout ',name))
                           ,@body))))))))

(defmacro testing (desc &body body)
  `(with-testing-with-options ,desc () ,@body))

(defmacro failing (desc &body body)
  "Execute BODY expecting all assertions to fail. If any assertion passes,
   the failing block itself is marked as failed."
  (let ((context (gensym "CONTEXT"))
        (unexpected-passes (gensym "UNEXPECTED-PASSES")))
    `(progn
       (test-begin *stats* ,desc)
       (unwind-protect
            (with-context (,context :name ,desc :description ,desc)
              ,@body
              ;; Collect unexpected passes
              (let ((,unexpected-passes (coerce (stats-passed-tests ,context) 'list)))
                ;; Clear the context stats - we'll handle reporting differently
                (setf (slot-value ,context 'rove/core/stats::passed)
                      (make-array 0 :adjustable t :fill-pointer 0))
                (setf (slot-value ,context 'rove/core/stats::failed)
                      (make-array 0 :adjustable t :fill-pointer 0))
                ;; Change unexpected passes to failures and record them
                (let ((*suppress-assertion-printing* t))
                  (dolist (assertion ,unexpected-passes)
                    ;; Change class from passed-assertion to failed-assertion
                    (change-class assertion 'failed-assertion)
                    ;; Update description
                    (setf (slot-value assertion 'rove/core/result::desc)
                          (format nil "Expected to fail: ~A"
                                  (assertion-description assertion)))
                    ;; Record in parent context as a failure
                    (record *stats* assertion)))))
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
