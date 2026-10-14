(defpackage #:rove/tests/report-sample
  (:use #:cl #:rove))
(in-package #:rove/tests/report-sample)

;; Deliberately mixed results; run by rove/tests/report through :junit-file.

(deftest sample-pass
  (ok (= 1 1)))

(deftest sample-fail
  (ok (equal (list 1 2) (list 1 3))))

(deftest sample-nested
  (testing "inner"
    (ok (= 1 1))
    (ok (= (length (list 1 2)) 3))))

(deftest sample-skip
  (skip "not yet"))
