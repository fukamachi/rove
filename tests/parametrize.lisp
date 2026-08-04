(defpackage #:rove/tests/parametrize
  (:use #:cl #:rove)
  (:import-from #:rove/core/suite/package #:get-test))
(in-package #:rove/tests/parametrize)

(deftest-parametrize add-static
    ((a b expected)
     (1 2 3)
     (0 0 0)
     (-1 1 0))
  (ok (= (+ a b) expected)))

(deftest-parametrize single-var
    ((n)
     1
     (2)
     3)
  (ok (plusp n)))

(deftest-parametrize with-ids
    ((n expected) :ids ("one" "two")
     (1 1)
     (2 2))
  (ok (= n expected)))

(defparameter *dyn-rows* '((10 20 30) (1 1 2)))

(deftest-parametrize add-dynamic
    ((a b expected) :rows *dyn-rows*)
  (ok (= (+ a b) expected)))

(deftest parametrize-names-registered
  (ok (functionp (get-test 'add-static/0)))
  (ok (functionp (get-test 'add-static/2)))
  (ok (functionp (get-test 'with-ids/one)))
  (ok (functionp (get-test 'add-dynamic/1)))
  (ok (equal 'add-static/0 (parametrize-test-name 'add-static 0)))
  (ok (equal 'with-ids/one (parametrize-test-name 'with-ids 0 "one"))))
