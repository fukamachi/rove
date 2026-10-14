(in-package #:cl-user)
(defpackage #:rove/reporter/junit
  (:use #:cl
        #:rove/reporter
        #:rove/core/stats
        #:rove/core/result)
  (:export #:junit-reporter
           #:write-junit-report
           #:write-junit-file))
(in-package #:rove/reporter/junit)

(defclass junit-reporter (reporter) ())

(defun xml-char-p (char)
  "XML 1.0 Char production; anything else is dropped from the output."
  (let ((code (char-code char)))
    (or (= code #x9) (= code #xA) (= code #xD)
        (<= #x20 code #xD7FF)
        (<= #xE000 code #xFFFD)
        (<= #x10000 code #x10FFFF))))

(defun xml-escape (object)
  (with-output-to-string (out)
    (loop for c across (princ-to-string object)
          when (xml-char-p c)
            do (case c
                 (#\< (write-string "&lt;" out))
                 (#\> (write-string "&gt;" out))
                 (#\& (write-string "&amp;" out))
                 (#\" (write-string "&quot;" out))
                 (#\' (write-string "&apos;" out))
                 (t (write-char c out))))))

(defun children (test)
  (append (passed-tests test)
          (failed-tests test)
          (pending-tests test)))

(defun child-tests (test)
  (remove-if-not (lambda (object) (typep object 'test)) (children test)))

(defun leaf-assertions (test)
  "All assertions recorded under TEST, including those in nested TESTING blocks."
  (loop for object in (children test)
        if (typep object 'test)
          append (leaf-assertions object)
        else
          collect object))

(defun test-seconds (test)
  (/ (loop for assertion in (leaf-assertions test)
           sum (or (assertion-duration assertion) 0))
     1000.0))

(defun test-classname (test default)
  (let ((name (test-name test)))
    (if (and (symbolp name) (symbol-package name))
        (string-downcase (package-name (symbol-package name)))
        default)))

(defun testcase-name (test)
  (let ((name (test-name test)))
    (or (test-description test)
        (if (symbolp name)
            (let ((*print-case* :downcase)) (princ-to-string name))
            name))))

(defun collect-testcases (objects classname)
  "Return (classname . test) pairs, one per DEFTEST. Contexts named by a
string (systems, suites) are descended into and supply the classname;
nested TESTING blocks stay inside their DEFTEST."
  (loop for object in objects
        when (typep object 'test)
          append (cond
                   ((symbolp (test-name object))
                    (list (cons (test-classname object classname) object)))
                   ((child-tests object)
                    (collect-testcases (children object) (test-name object)))
                   (t
                    (list (cons classname object))))))

(defun group-by-classname (testcases)
  (let ((groups '()))
    (dolist (testcase testcases)
      (let ((group (assoc (car testcase) groups :test #'string=)))
        (if group
            (push (cdr testcase) (cdr group))
            (push (list (car testcase) (cdr testcase)) groups))))
    (mapcar (lambda (group) (cons (car group) (reverse (cdr group))))
            (nreverse groups))))

(defun testcase-status (test)
  (let ((assertions (leaf-assertions test)))
    (cond
      ((some (lambda (a) (typep a 'failed)) assertions) :failed)
      ((and assertions (every (lambda (a) (typep a 'pending)) assertions)) :skipped)
      (t :passed))))

(defun failure-text (test)
  (with-output-to-string (out)
    (dolist (assertion (leaf-assertions test))
      (when (typep assertion 'failed)
        (format out "~A~%" (assertion-description assertion))
        (when (assertion-reason assertion)
          (format out "~A~%" (assertion-reason assertion)))
        (when (assertion-diff assertion)
          (format out "~A~%" (assertion-diff assertion)))))))

(defun write-testcase (stream classname test)
  (let ((status (testcase-status test)))
    (format stream "    <testcase classname=\"~A\" name=\"~A\" time=\"~,3F\""
            (xml-escape classname)
            (xml-escape (testcase-name test))
            (test-seconds test))
    (ecase status
      (:passed
       (format stream "/>~%"))
      (:skipped
       (format stream ">~%      <skipped/>~%    </testcase>~%"))
      (:failed
       (let ((first (find-if (lambda (a) (typep a 'failed)) (leaf-assertions test))))
         (format stream ">~%      <failure message=\"~A\">~A</failure>~%    </testcase>~%"
                 (xml-escape (assertion-description first))
                 (xml-escape (failure-text test))))))))

(defun count-status (tests status)
  (count status tests :key #'testcase-status))

(defun write-junit-report (stream stats &key (name "rove"))
  "Write STATS as JUnit XML: one <testsuite> per package, one <testcase> per DEFTEST."
  (let* ((testcases (collect-testcases (reverse (stats-results stats)) name))
         (groups (group-by-classname testcases))
         (tests (mapcar #'cdr testcases)))
    (format stream "<?xml version=\"1.0\" encoding=\"UTF-8\"?>~%")
    (format stream "<testsuites name=\"~A\" tests=\"~D\" failures=\"~D\" skipped=\"~D\" time=\"~,3F\">~%"
            (xml-escape name)
            (length tests)
            (count-status tests :failed)
            (count-status tests :skipped)
            (reduce #'+ tests :key #'test-seconds :initial-value 0.0))
    (loop for (classname . suite-tests) in groups
          do (format stream "  <testsuite name=\"~A\" tests=\"~D\" failures=\"~D\" skipped=\"~D\" time=\"~,3F\">~%"
                     (xml-escape classname)
                     (length suite-tests)
                     (count-status suite-tests :failed)
                     (count-status suite-tests :skipped)
                     (reduce #'+ suite-tests :key #'test-seconds :initial-value 0.0))
             (dolist (test suite-tests)
               (write-testcase stream classname test))
             (format stream "  </testsuite>~%"))
    (format stream "</testsuites>~%")))

(defun write-junit-file (pathname stats)
  (ensure-directories-exist pathname)
  (with-open-file (out pathname :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create)
    (write-junit-report out stats))
  (values))

(defmethod summarize ((reporter junit-reporter))
  (write-junit-report (reporter-stream reporter) reporter))
