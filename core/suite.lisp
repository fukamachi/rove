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

(defgeneric run-system (system)
  (:method ((system symbol))
    (run-system (asdf:find-system system)))
  (:method ((system string))
    (run-system (asdf:find-system system)))
  (:method ((system asdf:system))
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
                    (run-suite suite)))))

            (when package
              (let ((suite (package-suite package)))
                (when suite
                  (run-suite suite))))))
        (otherwise
          (dolist (suite (system-suites system))
            (run-suite suite)))))
    (system-tests-finish *stats* system)))

(defun run-system-tests (system-designators)
  (let ((system-designators (if (listp system-designators)
                                system-designators
                                (list system-designators))))
    (let ((*stats* (or *stats*
                       (make-instance 'stats)))
          (*standard-output* (or *rove-standard-output*
                                 *standard-output*))
          (*error-output* (or *rove-error-output*
                              *error-output*)))

      (initialize *stats*)

      (dolist (system system-designators)
        (run-system system))

      (summarize *stats*)

      (setf *last-suite-report* (stats-results *stats*))
      (values (passedp *stats*)
              (stats-results *stats*)))))
