(defpackage :rove/tests/main
  (:use :cl))
(in-package :rove/tests/main)

(rove:deftest example-test
  42)

(defun testing-returns-the-body-result ()
  (let ((expected 300)
        (actual (rove:testing "hello" 100 200 300)))
    (assert (equal expected actual))))

(defun run-test-returns-the-body-result ()
  (let ((expected 42)
        (actual (rove:run-test 'example-test)))
    (assert (equal expected actual))))

(rove:deftest test-failing-with-all-failures
  (rove:testing "failing block where all assertions fail (should pass)"
    (rove:failing "TODO: implement feature X"
      (rove:ok (= 1 0))
      (rove:ok (char= #\a #\b)))))

(rove:deftest test-failing-with-unexpected-success
  (rove:testing "failing block where assertions pass (should fail)"
    (rove:failing "This should fail but doesn't"
      (rove:ok (= 1 1)))))

(rove:deftest test-failing-with-mixed-results
  (rove:testing "failing block with mixed results (should fail because some pass)"
    (rove:failing "Mixed results"
      (rove:ok (= 1 1))
      (rove:ok (= 1 0)))))

(rove:deftest test-failing-with-no-assertions
  (rove:testing "failing block with no assertions (should fail)"
    (rove:failing "Empty block"
      nil)))

(defun run-all-tests ()
  (testing-returns-the-body-result)
  (assert (rove:run :rove/tests/parametrize)))
