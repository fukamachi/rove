(defpackage #:rove/tests/fixture-sample
  (:use #:cl #:rove)
  (:export #:*events*))
(in-package #:rove/tests/fixture-sample)

;; Records the fixture lifecycle; run by rove/tests/fixture through RUN.

(defvar *events* '())

(defun note (event)
  (setf *events* (append *events* (list event))))

(deffixture per-test ()
  (note :test-setup)
  (yield :test)
  (note :test-teardown))

(deffixture per-suite (:scope :suite)
  (note :suite-setup)
  (yield :suite)
  (note :suite-teardown))

(deffixture per-session (:scope :session)
  (note :session-setup)
  (yield :session)
  (note :session-teardown))

(setup (note :setup))
(teardown (note :teardown))

(deftest first-use
  (with-fixtures ((a per-test) (b per-suite) (c per-session))
    (note :test-1)
    (ok (eq a :test))
    (ok (eq b :suite))
    (ok (eq c :session))))

(deftest second-use
  (with-fixtures ((a per-test) (b per-suite) (c per-session))
    (note :test-2)
    (ok (eq a :test))
    (ok (eq b :suite))
    (ok (eq c :session))))
