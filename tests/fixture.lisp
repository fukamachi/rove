(defpackage #:rove/tests/fixture
  (:use #:cl #:rove))
(in-package #:rove/tests/fixture)

(defvar *fx-count* 0)
(defvar *fx-teardown* 0)

(deffixture bump-counter ()
  (incf *fx-count*)
  (yield *fx-count*)
  (incf *fx-teardown*))

(deffixture suite-token (:scope :suite)
  (yield :once))

(deftest fixture-test-scope
  (let ((*fx-count* 0)
        (*fx-teardown* 0))
    (with-fixture (n bump-counter)
      (ok (= n 1)))
    (ok (= *fx-count* 1))
    (ok (= *fx-teardown* 1))
    (with-fixtures ((a bump-counter) (b bump-counter))
      (ok (= a 2))
      (ok (= b 3)))
    (ok (= *fx-teardown* 3))))

(deftest fixture-suite-scope
  (with-fixture (a suite-token)
    (with-fixture (b suite-token)
      (ok (eq a :once))
      (ok (eq b :once)))))
