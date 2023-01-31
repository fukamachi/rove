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

(defun run-all-tests ()
  (testing-returns-the-body-result))
