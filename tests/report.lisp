(defpackage #:rove/tests/report
  (:use #:cl #:rove)
  (:import-from #:rove/core/diff
                #:format-value-diff
                #:maybe-value-diff)
  (:import-from #:rove/reporter/junit
                #:write-junit-report
                #:junit-reporter)
  (:import-from #:rove/core/stats
                #:*stats*
                #:stats
                #:with-context)
  (:import-from #:rove/core/result
                #:assertion-diff
                #:failed-tests))
(in-package #:rove/tests/report)

(defun record-under-fresh-stats (thunk)
  "Run THUNK with its own stats so deliberate failures do not fail this suite.
Returns the failed assertions it recorded."
  (let ((*stats* (make-instance 'stats))
        (failed nil))
    (with-context (context :name "probe")
      (funcall thunk)
      (setf failed (failed-tests context)))
    failed))

(defmacro failed-diff (form)
  "Diff recorded for FORM when asserted with OK and failing."
  `(let ((failed (record-under-fresh-stats (lambda () (ok ,form)))))
     (and failed (assertion-diff (first failed)))))

(deftest value-diff-text
  (let ((diff (format-value-diff "a" "b")))
    (ok (search "expected: \"a\"" diff))
    (ok (search "actual:   \"b\"" diff)))
  (ok (search "expected:" (maybe-value-diff '(equal x y) '("aa" "bb"))))
  (ng (maybe-value-diff '(plusp n) '(1)))
  (ng (maybe-value-diff '(equal x y) '("aa" "bb" "cc"))))

(deftest line-diff-text
  (let ((diff (format-value-diff (format nil "a~%b~%c") (format nil "a~%c~%d"))))
    (ok (search "--- expected" diff))
    (ok (search (format nil "~%     a") diff) "common line kept")
    (ok (search (format nil "~%    -b") diff) "removed line marked")
    (ok (search (format nil "~%     c") diff) "line shifted by the removal still matches")
    (ok (search (format nil "~%    +d") diff) "added line marked")))

(deftest diff-print-limits
  (let ((diff (format-value-diff (make-list 1000 :initial-element 0) nil)))
    (ok (search "..." diff)))
  (let ((*print-length* nil)
        (rove/core/diff:*diff-print-length* nil))
    (ng (search "..." (format-value-diff (make-list 1000 :initial-element 0) nil)))))

(deftest diff-recorded-on-failed-comparisons
  (let ((expected "a") (actual "b"))
    (ok (search "expected: \"a\"" (failed-diff (equal expected actual)))))
  (let ((list '(1 2)))
    (ok (search "expected: (2 1)" (failed-diff (equal (reverse list) '(1 2))))
        "nested call operands get a diff")
    (ok (search "actual:   3" (failed-diff (= (length list) 3))))
    (ok (search "expected: (1 2)" (failed-diff (equal (list 1 2) (list 1 3))))
        "both operands nested")))

(deftest diff-not-recorded-when-meaningless
  (ng (failed-diff (plusp -1)) "not a comparison")
  (let ((failed (record-under-fresh-stats (lambda () (ng (equal "same" "same"))))))
    (ng (assertion-diff (first failed)) "negative assertions carry no expected/actual"))
  (let ((failed (record-under-fresh-stats
                 (lambda () (let ((*debug-on-error* nil)) (ok (equal (error "boom") 1)))))))
    (ng (assertion-diff (first failed)) "errors carry no expected/actual")))

(defun run-sample-junit ()
  (let ((path (merge-pathnames (format nil "rove-junit-~D.xml" (get-universal-time))
                               (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (run :rove/tests/report-sample :style :none :junit-file path)
           (uiop:read-file-string path))
      (uiop:delete-file-if-exists path))))

(deftest junit-file-written-for-any-style
  (let ((xml (run-sample-junit)))
    (ok (search "<?xml" xml))
    (ok (search "<testsuites name=\"rove\" tests=\"4\" failures=\"2\" skipped=\"1\"" xml))
    (ok (search "<testsuite name=\"rove/tests/report-sample\" tests=\"4\" failures=\"2\" skipped=\"1\"" xml))
    (ok (search "<testcase classname=\"rove/tests/report-sample\" name=\"sample-pass\"" xml))
    (ok (search "name=\"sample-fail\"" xml))
    (ok (search "name=\"sample-nested\"" xml) "nested testing is folded into its deftest")
    (ng (search "name=\"inner\"" xml))
    (ng (search "name=\"rove/tests/report-sample\" time" xml) "suite is not reported as a testcase")
    (ok (search "<skipped/>" xml))
    (ok (search "<failure message=\"Expect (EQUAL (LIST 1 2) (LIST 1 3)) to be true.\">" xml))
    (ok (search "expected: (1 2)" xml) "diff is included in the failure body")
    (ok (search "actual:   (1 3)" xml))
    (ok (search "Expect (= (LENGTH (LIST 1 2)) 3) to be true." xml))))

(deftest junit-style-prints-xml
  (let ((xml (with-output-to-string (*standard-output*)
               (run :rove/tests/report-sample :style :junit))))
    (ok (search "<?xml" xml))
    (ok (search "name=\"sample-fail\"" xml))))

(deftest junit-xml-escaping
  (let ((reporter (make-instance 'junit-reporter :stream (make-broadcast-stream))))
    (rove/core/stats:record reporter
                            (make-instance 'rove/core/result:passed-test
                                           :name (format nil "a<b>&\"c\"~C~C" (code-char 27) (code-char 0))
                                           :description nil))
    (let ((xml (with-output-to-string (s) (write-junit-report s reporter))))
      (ok (search "name=\"a&lt;b&gt;&amp;&quot;c&quot;\"" xml) "escaped, control characters dropped")
      (ng (find (code-char 27) xml))
      (ng (find (code-char 0) xml)))))
