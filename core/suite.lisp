(in-package #:cl-user)
(defpackage #:rove/core/suite
  (:use #:cl
        #:rove/core/assertion
        #:rove/core/result
        #:rove/core/test
        #:rove/core/stats)
  (:import-from #:rove/core/result
                #:test-name)
  (:import-from #:rove/core/suite/package
                #:*execute-assertions*
                #:system-suites
                #:suite-name
                #:run-suite)
  (:import-from #:rove/core/suite/file
                #:system-packages)
  (:export #:run-system-tests
           #:*last-suite-report*
           #:*rove-standard-output*
           #:*rove-error-output*))
(in-package #:rove/core/suite)

(defvar *last-suite-report* nil)

(defparameter *rove-standard-output* nil)
(defparameter *rove-error-output* nil)

(defun run-system-tests (system-designator)
  (let ((system (asdf:find-system system-designator)))
    (let ((*stats* (or *stats*
                       (make-instance 'stats)))
          (*execute-assertions* nil)
          (*standard-output* (or *rove-standard-output*
                                 *standard-output*))
          (*error-output* (or *rove-error-output*
                              *error-output*)))

      #+quicklisp (ql:quickload (asdf:component-name system) :silent t)
      #-quicklisp (asdf:load-system (asdf:component-name system))

      (let ((*execute-assertions* t))
        (testing (format nil "Testing System ~A" (asdf:component-name system))
          (typecase system
            (asdf:package-inferred-system
             (let* ((package-name (string-upcase (asdf:component-name system)))
                    (package (find-package package-name)))
               (unless package
                 (setf package (find-package package-name)))
               ;; Loading dependencies beforehand
               (let ((pkgs (system-packages system)))
                 (dolist (package pkgs)
                   (when (package-tests package)
                     (format t "~2&;; testing '~(~A~)'~%" (package-name package))
                     (run-package-tests package))))

               (when (and package
                          (package-tests package))
                 (format t "~2&;; testing '~(~A~)'~%" (package-name package))
                 (run-package-tests package))))
            (otherwise
             (dolist (suite (system-suites system))
               (format t "~2&;; testing '~(~A~)'~%" (suite-name suite))
               (run-suite suite))))))

      (let ((test (if (/= (length (stats-failed *stats*)) 0)
                      (aref (stats-failed *stats*) 0)
                      (aref (stats-passed *stats*) 0))))
        (let ((passed (test-passed-assertions test))
              (failed (test-failed-assertions test)))

          (format t "~2&Summary:~%")
          (if failed
              (format t "  ~D file~:*~P failed.~{~%    - ~A~}~%"
                      (length failed)
                      (mapcar #'test-name failed))
              (format t "  All ~D file~:*~P passed.~%"
                      (length passed)))

          (setf *last-suite-report*
                (list (= 0 (length failed))
                      passed
                      failed))
          (apply #'values *last-suite-report*))))))
