(defpackage #:rove/tests/marks
  (:use #:cl #:rove)
  (:import-from #:rove/core/marks
                #:test-marks
                #:normalize-mark-expr
                #:eval-mark-expr
                #:test-selected-by-marks-p
                #:*mark-expr*)
  (:import-from #:rove/core/suite/package
                #:selected-suite-tests
                #:run-suite-tests
                #:suite-tests
                #:package-suite
                #:check-shard
                #:test-shard
                #:*shard*
                #:*shard-count*)
  (:import-from #:rove/core/stats
                #:*stats*
                #:stats
                #:stats-results)
  (:import-from #:rove/core/result
                #:test-name
                #:failed-test
                #:failed-tests
                #:assertion-form)
  (:import-from #:rove/core/test
                #:test-timeout))
(in-package #:rove/tests/marks)

(deftest (marked-slow :marks (:slow))
  (ok t))

(deftest (marked-fast :marks :fast)
  (ok t))

(deftest (marked-timeout :timeout 30)
  (ok t))

(deftest mark-metadata
  (ok (equal '(:slow) (test-marks 'marked-slow)))
  (ok (equal '(:fast) (test-marks 'marked-fast)))
  (ok (null (test-marks 'mark-metadata)))
  (ok (eval-mark-expr :slow '(:slow)))
  (ng (eval-mark-expr :slow '(:fast)))
  (ok (eval-mark-expr '(not :slow) '(:fast)))
  (ok (eval-mark-expr '(or :slow :fast) '(:fast)))
  (ng (eval-mark-expr '(and :slow :fast) '(:slow)))
  (ok (eval-mark-expr '(:and "slow" db) '(:slow :db)))
  (let ((*mark-expr* :slow))
    (ok (test-selected-by-marks-p 'marked-slow))
    (ng (test-selected-by-marks-p 'marked-fast)))
  (let ((*mark-expr* '(not :slow)))
    (ng (test-selected-by-marks-p 'marked-slow))
    (ok (test-selected-by-marks-p 'marked-fast))))

(deftest mark-expr-validation
  (ok (eq t (normalize-mark-expr nil)))
  (ok (equal '(and :slow (not :db)) (normalize-mark-expr '(:and slow (not "db")))))
  (dolist (expr '((not) (not :a :b) (and) (or) (xor :a) 42 (:a :b) (and nil)))
    (ok (signals (normalize-mark-expr expr) 'error) (format nil "~S is rejected" expr)))
  (ok (signals (run :rove/tests/marks :marks '(not)) 'error)))

(deftest timeout-metadata
  (ok (= 30 (test-timeout 'marked-timeout)))
  (ok (null (test-timeout 'mark-metadata)))
  (let ((*default-test-timeout* 7))
    (ok (= 7 (test-timeout 'mark-metadata)))
    (ok (= 30 (test-timeout 'marked-timeout))))
  (ok (signals (eval '(deftest (bad-timeout :timeout "5") (ok t))) 'type-error))
  (ok (signals (eval '(deftest (bad-timeout :timeout 0) (ok t))) 'type-error)))

(deftest redefinition-resets-options
  (let ((name (intern (string (gensym "REDEF")) :rove/tests/marks)))
    (unwind-protect
         (progn
           (eval `(deftest (,name :marks (:slow) :timeout 3) (ok t)))
           (ok (equal '(:slow) (test-marks name)))
           (ok (= 3 (test-timeout name)))
           (eval `(deftest ,name (ok t)))
           (ok (null (test-marks name)))
           (ok (null (test-timeout name))))
      (remove-test name))))

(deftest shard-validation
  (ok (null (check-shard nil nil)))
  (ok (null (check-shard 0 1)))
  (ok (null (check-shard 3 4)))
  (dolist (args '((4 4) (-1 4) (0 nil) (nil 4) (0 "4") ("0" 4) (1.0 4)))
    (ok (signals (apply #'check-shard args) 'error) (format nil "~S is rejected" args)))
  (ok (signals (run :rove/tests/marks :shard 4 :shards 4) 'error))
  (ok (signals (run :rove/tests/marks :shards "4") 'error)))

(deftest shard-assignment
  (let* ((suite (package-suite *package*))
         (tests (suite-tests suite))
         (shards 3))
    (ok (every (lambda (name) (< -1 (test-shard name shards) shards)) tests))
    (ok (= (test-shard 'marked-slow shards) (test-shard 'marked-slow shards)))
    (let ((selected (loop for shard below shards
                          collect (let ((*mark-expr* nil)
                                        (*shard* shard)
                                        (*shard-count* shards))
                                    (selected-suite-tests suite)))))
      (ok (= (length tests) (reduce #'+ selected :key #'length)))
      (ok (null (set-difference tests (reduce #'append selected))))
      (ok (loop for (a . rest) on selected
                always (notany (lambda (b) (intersection a b)) rest))))))

(deftest selected-tests-by-marks
  (let* ((suite (package-suite *package*))
         (*mark-expr* :slow)
         (*shard* nil)
         (*shard-count* nil)
         (marked (selected-suite-tests suite)))
    (ok (member 'marked-slow marked))
    (ng (member 'marked-fast marked))))

(defun run-under-fresh-stats (thunk)
  (let ((*stats* (make-instance 'stats)))
    (funcall thunk)
    *stats*))

(deftest timeout-records-failure
  (let ((name (intern (string (gensym "TO")) :rove/tests/marks))
        (*debug-on-error* nil))
    (eval `(deftest (,name :timeout 0.05)
             (sleep 1)
             (ok t)))
    (unwind-protect
         (progn
           (ng (run-test name :style :none))
           (let* ((stats (run-under-fresh-stats (lambda () (funcall (get-test name)))))
                  (result (first (stats-results stats))))
             (ok (= 1 (length (stats-results stats))))
             (ok (typep result 'failed-test))
             (ok (eq name (test-name result)))
             (ok (= 1 (length (failed-tests result))))
             (ok (eq 'rove/core/test::timeout (first (assertion-form (first (failed-tests result)))))))
           (let ((*quit-on-failure* t))
             (ok (signals (run-under-fresh-stats (lambda () (funcall (get-test name))))
                          'quit-early))))
      (remove-test name))))

(defpackage #:rove/tests/marks/fixture
  (:use #:cl #:rove))
(in-package #:rove/tests/marks/fixture)

(defvar *setup-count* 0)
(defvar *teardown-count* 0)

(setup (incf *setup-count*))
(teardown (incf *teardown-count*))

(deftest (fixture-slow :marks :slow)
  (ok t))

(in-package #:rove/tests/marks)

(deftest suite-skipped-when-nothing-selected
  (let ((suite (package-suite :rove/tests/marks/fixture)))
    (flet ((run-fixture (mark-expr)
             (let ((before rove/tests/marks/fixture::*setup-count*)
                   (*mark-expr* mark-expr))
               (run-under-fresh-stats (lambda () (run-suite-tests suite)))
               (- rove/tests/marks/fixture::*setup-count* before))))
      (ok (= 1 (run-fixture nil)))
      (ok (= 1 (run-fixture :slow)))
      (ok (= 0 (run-fixture :fast)))
      (ok (= rove/tests/marks/fixture::*setup-count*
             rove/tests/marks/fixture::*teardown-count*)))))
