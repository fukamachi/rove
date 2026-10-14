(defpackage #:rove/tests/fixture
  (:use #:cl #:rove)
  (:import-from #:rove/core/fixture
                #:*suite-fixture-cache*
                #:*session-fixture-cache*)
  (:import-from #:rove/core/suite/package
                #:get-test
                #:remove-test)
  (:import-from #:rove/core/stats
                #:*stats*
                #:stats
                #:with-context)
  (:import-from #:rove/tests/fixture-sample
                #:*events*))
(in-package #:rove/tests/fixture)

(defvar *fx-count* 0)
(defvar *fx-teardown* 0)

(deffixture bump-counter ()
  (incf *fx-count*)
  (yield *fx-count*)
  (incf *fx-teardown*))

(deffixture bound-resource ()
  (let ((resource (list :open)))
    (yield resource)
    (setf (car resource) :closed)))

(deftest fixture-test-scope
  (let ((*fx-count* 0)
        (*fx-teardown* 0))
    (with-fixture (n bump-counter)
      (ok (= n 1))
      (ok (= *fx-teardown* 0) "teardown has not run inside the body"))
    (ok (= *fx-count* 1))
    (ok (= *fx-teardown* 1))
    (with-fixtures ((a bump-counter) (b bump-counter))
      (ok (= a 2))
      (ok (= b 3)))
    (ok (= *fx-teardown* 3))))

(deftest fixture-teardown-keeps-lexical-bindings
  (let ((seen nil))
    (with-fixture (r bound-resource)
      (setf seen r)
      (ok (equal '(:open) r)))
    (ok (equal '(:closed) seen) "teardown after YIELD inside LET sees the binding")))

(deftest fixture-teardown-runs-on-non-local-exit
  (let ((*fx-count* 0)
        (*fx-teardown* 0))
    (ok (eq :escaped
            (catch 'escape
              (with-fixture (n bump-counter)
                (declare (ignore n))
                (throw 'escape :escaped)))))
    (ok (= *fx-teardown* 1) "teardown after THROW")
    (handler-case
        (with-fixture (n bump-counter)
          (declare (ignore n))
          (error "boom"))
      (error () nil))
    (ok (= *fx-teardown* 2) "teardown after ERROR")
    (let ((name (intern (string (gensym "TIMEOUT")) *package*)))
      (eval `(deftest (,name :timeout 0.05)
               (with-fixture (n bump-counter)
                 (declare (ignore n))
                 (sleep 1))))
      (unwind-protect
           (let ((*stats* (make-instance 'stats)))
             (with-context (context :name "probe")
               (funcall (get-test name))))
        (remove-test name))
      (ok (= *fx-teardown* 3) "teardown after :timeout"))))

(defmacro rejected (form &optional (condition ''error))
  `(ok (signals (macroexpand-1 ',form) ,condition)
       ,(format nil "~S is rejected" form)))

(deftest deffixture-validates-definition
  (rejected (deffixture bad () (list 1)))
  (rejected (deffixture bad () (yield 1) (yield 2)))
  (rejected (deffixture bad () (when t (yield 1))))
  (rejected (deffixture bad () (let ((x 1)) (yield x)) (print :after-let)))
  (rejected (deffixture bad () (yield 1 2)))
  (rejected (deffixture bad (:scope :forever) (yield 1)) 'type-error)
  (rejected (deffixture "bad" () (yield 1)) 'type-error)
  (rejected (yield 1))
  (rejected (with-fixtures ((a)) a))
  (ok (signals (with-fixture (x no-such-fixture) x) 'error)))

(defvar *cache-log* '())

(deftest fixture-redefinition-evicts-cache
  (let ((name (intern (string (gensym "CACHED")) *package*))
        (*cache-log* '())
        (*suite-fixture-cache* '()))
    (eval `(deffixture ,name (:scope :suite)
             (push :setup-1 *cache-log*)
             (yield 1)
             (push :teardown-1 *cache-log*)))
    (ok (= 1 (eval `(with-fixture (v ,name) v))))
    (ok (= 1 (eval `(with-fixture (v ,name) v))) "cached within the suite")
    (ok (equal '(:setup-1) *cache-log*) "setup once, no teardown yet")
    (eval `(deffixture ,name (:scope :suite)
             (push :setup-2 *cache-log*)
             (yield 2)
             (push :teardown-2 *cache-log*)))
    (ok (equal '(:teardown-1 :setup-1) *cache-log*) "redefinition tears the old value down")
    (ok (= 2 (eval `(with-fixture (v ,name) v))) "new definition is used")
    (ok (equal '(:setup-2 :teardown-1 :setup-1) *cache-log*))))

(deftest fixture-suite-and-session-lifecycle
  (setf *events* '())
  (ok (run :rove/tests/fixture-sample :style :none))
  (ok (equal '(:setup
               :test-setup :suite-setup :session-setup :test-1 :test-teardown
               :test-setup :test-2 :test-teardown
               :teardown :suite-teardown :session-teardown)
             *events*))
  (setf *events* '())
  (ok (run-test 'rove/tests/fixture-sample::first-use :style :none))
  (ok (equal '(:test-setup :suite-setup :session-setup :test-1 :test-teardown
               :session-teardown :suite-teardown)
             *events*)
      "run-test tears cached fixtures down when it finishes"))
