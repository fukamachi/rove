(defpackage #:rove/tests/parametrize
  (:use #:cl #:rove)
  (:import-from #:rove/core/suite/package
                #:get-test
                #:suite-tests
                #:package-suite
                #:remove-test)
  (:import-from #:rove/core/marks
                #:test-marks)
  (:import-from #:rove/core/test
                #:test-timeout))
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

(deftest-parametrize dynamic-with-literal-ids
    ((a b expected) :ids ("first" "second") :rows *dyn-rows*)
  (ok (= (+ a b) expected)))

(defparameter *dyn-ids* '("alpha" "beta"))

(deftest-parametrize dynamic-with-evaluated-ids
    ((a b expected) :ids *dyn-ids* :rows *dyn-rows*)
  (ok (= (+ a b) expected)))

(deftest-parametrize (marked-rows :marks (:slow) :timeout 30)
    ((n) 1 2)
  (ok (plusp n)))

(deftest parametrize-names-registered
  (ok (functionp (get-test 'add-static/0)))
  (ok (functionp (get-test 'add-static/2)))
  (ok (functionp (get-test 'with-ids/one)))
  (ok (functionp (get-test 'add-dynamic/1)))
  (ok (functionp (get-test 'dynamic-with-literal-ids/first)))
  (ok (functionp (get-test 'dynamic-with-evaluated-ids/beta)))
  (ok (equal 'add-static/0 (parametrize-test-name 'add-static 0)))
  (ok (equal 'with-ids/one (parametrize-test-name 'with-ids 0 "one"))))

(deftest parametrize-passes-deftest-options
  (ok (equal '(:slow) (test-marks 'marked-rows/0)))
  (ok (equal '(:slow) (test-marks 'marked-rows/1)))
  (ok (= 30 (test-timeout 'marked-rows/1))))

(defmacro rejected (form)
  `(ok (signals (macroexpand-1 ',form) 'error)
       ,(format nil "~S is rejected" (third form))))

(deftest parametrize-rejects-invalid-definitions
  (rejected (deftest-parametrize bad ((n) :ids ("a") 1 2) (ok n)))
  (rejected (deftest-parametrize bad ((n) :ids ("a" "A") 1 2) (ok n)))
  (rejected (deftest-parametrize bad ((n n) (1 2)) (ok n)))
  (rejected (deftest-parametrize bad ((n :key) (1 2)) (ok n)))
  (rejected (deftest-parametrize bad ((a b) (1 2) (3)) (ok a)))
  (rejected (deftest-parametrize bad ((n) :foo 1 1) (ok n)))
  (rejected (deftest-parametrize bad ((n) :rows *dyn-rows* 1) (ok n)))
  (rejected (deftest-parametrize bad ((n) :ids) (ok n)))
  (rejected (deftest-parametrize bad ((n)) (ok n)))
  (rejected (deftest-parametrize bad (42 1) (ok n)))
  (ok (signals (eval '(deftest-parametrize bad ((n) :ids ("a") :rows '((1) (2))) (ok n)))
               'error)
      ":ids length mismatch with :rows is rejected at load time"))

(deftest parametrize-redefinition-drops-stale-rows
  (let ((base (intern (string (gensym "SHRINK")) *package*)))
    (unwind-protect
         (progn
           (eval `(deftest-parametrize ,base ((n) 1 2 3) (ok (plusp n))))
           (ok (functionp (get-test (parametrize-test-name base 2))))
           (eval `(deftest-parametrize ,base ((n) 1) (ok (plusp n))))
           (ok (functionp (get-test (parametrize-test-name base 0))))
           (ng (get-test (parametrize-test-name base 1)))
           (ng (get-test (parametrize-test-name base 2)))
           (ng (member (parametrize-test-name base 2) (suite-tests (package-suite *package*))))
           (eval `(deftest-parametrize ,base ((n) :rows '((5) (6))) (ok (plusp n))))
           (ok (functionp (get-test (parametrize-test-name base 1))))
           (eval `(deftest-parametrize ,base ((n) :ids ("only") 7) (ok (plusp n))))
           (ok (functionp (get-test (parametrize-test-name base 0 "only"))))
           (ng (get-test (parametrize-test-name base 0)))
           (ng (get-test (parametrize-test-name base 1))))
      (dolist (index '(0 1 2))
        (remove-test (parametrize-test-name base index)))
      (remove-test (parametrize-test-name base 0 "only")))))
