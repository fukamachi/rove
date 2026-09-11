(defpackage #:rove/tests/report
  (:use #:cl #:rove)
  (:import-from #:rove/core/diff
                #:format-value-diff
                #:maybe-value-diff)
  (:import-from #:rove/reporter/junit
                #:write-junit-report
                #:junit-reporter)
  (:import-from #:rove/core/stats
                #:record)
  (:import-from #:rove/core/result
                #:passed-test
                #:assertion-diff
                #:failed-assertion))
(in-package #:rove/tests/report)

(deftest value-diff-text
  (let ((diff (format-value-diff "a" "b")))
    (ok (search "expected:" diff))
    (ok (search "actual:" diff)))
  (ok (search "expected:" (maybe-value-diff '(equal x y) '("aa" "bb"))))
  (ng (maybe-value-diff '(plusp n) '(1))))

(deftest junit-xml-shape
  (let ((reporter (make-instance 'junit-reporter
                                 :stream (make-broadcast-stream))))
    (record reporter
            (make-instance 'passed-test
                           :name "ok-case"
                           :description "ok"))
    (let ((xml (with-output-to-string (s)
                 (write-junit-report s reporter :suite-name "report"))))
      (ok (search "<?xml" xml))
      (ok (search "testsuite" xml))
      (ok (search "ok-case" xml)))))

(deftest assertion-diff-slot
  (let ((diff (maybe-value-diff '(equal x y) '("aa" "bb"))))
    (ok (search "expected:"
                (assertion-diff
                 (make-instance 'failed-assertion
                                :form '(equal x y)
                                :values '("aa" "bb")
                                :diff diff))))))
