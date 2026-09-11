(in-package #:cl-user)
(defpackage #:rove/reporter/junit
  (:use #:cl
        #:rove/reporter
        #:rove/core/stats
        #:rove/core/result)
  (:export #:junit-reporter
           #:write-junit-report))
(in-package #:rove/reporter/junit)

(defclass junit-reporter (reporter)
  ((output-file :initarg :output-file
                :initform nil
                :accessor junit-reporter-output-file)))

(defun xml-escape (string)
  (with-output-to-string (out)
    (loop for c across (princ-to-string string)
          do (case c
               (#\< (write-string "&lt;" out))
               (#\> (write-string "&gt;" out))
               (#\& (write-string "&amp;" out))
               (#\" (write-string "&quot;" out))
               (#\' (write-string "&apos;" out))
               (t (write-char c out))))))

(defun assertion-seconds (assertion)
  (let ((ms (and (typep assertion 'assertion)
                 (assertion-duration assertion))))
    (if ms
        (/ ms 1000.0)
        0.0)))

(defun flatten-tests (reporter)
  (remove-if-not (lambda (object) (typep object 'test))
                 (stats-results reporter)))

(defun test-time (test)
  (loop for a in (append (passed-tests test)
                         (failed-tests test)
                         (pending-tests test))
        sum (if (typep a 'assertion)
                (assertion-seconds a)
                0.0)))

(defun failure-text (test)
  (with-output-to-string (m)
    (dolist (f (failed-tests test))
      (when (typep f 'assertion)
        (format m "~A" (assertion-description f))
        (when (assertion-diff f)
          (format m "~A" (assertion-diff f)))
        (terpri m)))))

(defun write-testcase (stream test classname)
  (let* ((failed (failed-tests test))
         (pending (pending-tests test))
         (name (or (test-name test) (test-description test) "unnamed")))
    (format stream "    <testcase classname=\"~A\" name=\"~A\" time=\"~,3F\""
            (xml-escape classname)
            (xml-escape name)
            (test-time test))
    (cond
      ((and pending (null failed)
            (null (passed-tests test)))
       (format stream ">~%      <skipped/>~%    </testcase>~%"))
      (failed
       (format stream ">~%      <failure message=\"~A\">~A</failure>~%    </testcase>~%"
               (xml-escape (let ((first (find-if (lambda (x) (typep x 'assertion)) failed)))
                             (if first
                                 (assertion-description first)
                                 "failed")))
               (xml-escape (failure-text test))))
      (t
       (format stream "/>~%")))))

(defun write-junit-report (stream reporter &key (suite-name "rove"))
  (let* ((passed (passed-tests reporter))
         (failed (failed-tests reporter))
         (pending (pending-tests reporter))
         (tests (+ (length passed) (length failed) (length pending)))
         (cases (flatten-tests reporter)))
    (format stream "<?xml version=\"1.0\" encoding=\"UTF-8\"?>~%")
    (format stream "<testsuites>~%")
    (format stream "  <testsuite name=\"~A\" tests=\"~D\" failures=\"~D\" skipped=\"~D\" time=\"0\">~%"
            (xml-escape suite-name)
            tests
            (length failed)
            (length pending))
    (dolist (test cases)
      (write-testcase stream test suite-name))
    (format stream "  </testsuite>~%")
    (format stream "</testsuites>~%")))

(defmethod summarize ((reporter junit-reporter))
  (let ((xml (with-output-to-string (s)
               (write-junit-report s reporter)))
        (file (or (junit-reporter-output-file reporter)
                  *junit-output-file*)))
    (write-string xml (reporter-stream reporter))
    (when file
      (with-open-file (out file :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create)
        (write-string xml out)))
    xml))
