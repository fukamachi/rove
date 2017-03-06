(in-package #:cl-user)
(defpackage #:rove/examples/passed
  (:use #:cl
        #:rove))
(in-package #:rove/examples/passed)

(deftest array-length
  (let ((ary #(1 2 3 4 5)))
    (ok (= (length ary) 5))
    (ng (= (length ary) 3))
    (ok (signals (length 1)))))

(deftest array-position
  (let ((ary #(1 2 3 4 5)))
    (ok (= (position 4 ary) 3))
    (ok (= (position-if #'evenp ary) 1)))
  (ng (eql (position-if #'evenp #(1 3 5)) 1)))
