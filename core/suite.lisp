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
                #:get-test
                #:remove-test
                #:system-suites
                #:suite-name
                #:run-suite)
  (:import-from #:rove/core/suite/file
                #:system-packages)
  (:export #:run-system-tests
           #:run-suite
           #:*last-suite-report*
           #:*rove-standard-output*
           #:*rove-error-output*
           #:get-test
           #:remove-test))
(in-package #:rove/core/suite)

(defvar *last-suite-report* nil)

(defparameter *rove-standard-output* nil)
(defparameter *rove-error-output* nil)

(define-condition system-tests-failure (asdf:test-op-test-failures)
  ((test-report :initarg :test-report
                :reader  system-tests-failure-report))
  (:report
   (lambda (condition stream)
     (write-string (system-tests-failure-report condition) stream))))

(defun run-system-tests (system-designator)
  (let ((system (asdf:find-system system-designator))
        (test-report-stream (make-string-output-stream)))
    (let ((*stats* (or *stats*
                       (make-instance 'stats)))
          (*standard-output* (make-broadcast-stream
                              (or *rove-standard-output*
                                  *standard-output*)
                              test-report-stream))
          (*error-output* (or *rove-error-output*
                              *error-output*)))

      #+quicklisp (ql:quickload (asdf:component-name system) :silent t)
      #-quicklisp (asdf:load-system (asdf:component-name system))

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
              (run-suite suite)))))

      (let ((test (if (/= (length (stats-failed *stats*)) 0)
                      (aref (stats-failed *stats*) 0)
                      (aref (stats-passed *stats*) 0))))
        (let* ((passed (test-passed-assertions test))
               (failed (test-failed-assertions test))
               (failed-test-names (mapcar #'test-name failed)))

          (format t "~2&Summary:~%")
          (if failed
              (format t "  ~D test~:*~P failed.~{~%    - ~A~}~%"
                      (length failed)
                      failed-test-names)
              (format t "  All ~D test~:*~P passed.~%"
                      (length passed)))

          (cond (failed
                 (signal 'system-tests-failure
                         :test-report       (get-output-stream-string
                                             test-report-stream)
                         :tests-run-count   (+ (length passed)
                                               (length failed))
                         :failed-test-names failed-test-names))
                ((= 0 (length passed))
                 (signal 'system-tests-failure
                         :test-report "No tests ran.")))

          (setf *last-suite-report*
                (list (= 0 (length failed))
                      passed
                      failed))
          (apply #'values *last-suite-report*))))))
