(in-package #:cl-user)
(defpackage #:rove/core/suite
  (:use #:cl
        #:rove/core/assertion
        #:rove/core/result
        #:rove/core/test
        #:rove/core/stats)
  (:import-from #:rove/core/suite/package
                #:get-test
                #:remove-test
                #:system-suites
                #:suite-name
                #:run-suite
                #:package-suite
                #:all-suites
                #:find-suite)
  (:import-from #:rove/core/suite/file
                #:system-packages)
  (:export #:run-suite
           #:run-system
           #:run-test-functions
           #:all-suites
           #:find-suite
           #:*last-suite-report*
           #:*rove-standard-output*
           #:*rove-error-output*
           #:get-test
           #:remove-test))
(in-package #:rove/core/suite)

(defvar *last-suite-report* nil)

(defparameter *rove-standard-output* nil)
(defparameter *rove-error-output* nil)

(defgeneric run-system (system &optional pred)
  (:method ((system symbol) &optional pred)
    (run-system (asdf:find-system system) pred))
  (:method ((system string) &optional pred)
    (run-system (asdf:find-system system) pred))
  (:method ((system asdf:system) &optional pred)
    #+quicklisp (ql:quickload (asdf:component-name system) :silent t)
    #-quicklisp (asdf:load-system (asdf:component-name system))

    (system-tests-begin *stats* system)
    (with-context (context :name (asdf:component-name system))
      (typecase system
        (asdf:package-inferred-system
          (let* ((package-name (string-upcase (asdf:component-name system)))
                 (package (find-package package-name)))
            (unless package
              (setf package (find-package package-name)))
            ;; Loading dependencies beforehand
            (let ((pkgs (system-packages system)))
              (dolist (package pkgs)
                (let ((suite (package-suite package)))
                  (when suite
                    (run-suite suite pred)))))

            (when package
              (let ((suite (package-suite package)))
                (when suite
                  (run-suite suite pred))))))
        (otherwise
          (dolist (suite (system-suites system))
            (run-suite suite pred)))))
    (system-tests-finish *stats* system)))

(defun call-with-suite (function)
  (let ((*stats* (or *stats*
                     (make-instance 'stats)))
        (*standard-output* (or *rove-standard-output*
                               *standard-output*))
        (*error-output* (or *rove-error-output*
                            *error-output*)))
    (initialize *stats*)

    (funcall function)

    (summarize *stats*)

    (setf *last-suite-report* (stats-results *stats*))
    (values (passedp *stats*)
            (stats-results *stats*))))

(defun run-system-tests (system-designators &optional pred)
  (let ((system-designators (if (listp system-designators)
                                system-designators
                                (list system-designators))))
    (call-with-suite
     (lambda ()
       (dolist (system system-designators)
         (run-system system pred))))))

(defun run-test-functions (test-functions)
  (call-with-suite
   (lambda ()
     (dolist (test test-functions)
       (funcall test)))))
