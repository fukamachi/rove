(defpackage #:rove/tests/marks
  (:use #:cl #:rove)
  (:import-from #:rove/core/marks
                #:test-marks
                #:eval-mark-expr
                #:test-selected-by-marks-p
                #:*mark-expr*)
  (:import-from #:rove/core/suite/package
                #:selected-suite-tests
                #:package-suite
                #:*shard*
                #:*shard-count*)
  (:import-from #:rove/core/test
                #:test-timeout))
(in-package #:rove/tests/marks)

(deftest (marked-slow :marks (:slow))
  (ok t))

(deftest (marked-fast :marks (:fast))
  (ok t))

(deftest (marked-timeout :timeout 30)
  (ok t))

(deftest mark-metadata
  (ok (equal '(:slow) (test-marks 'marked-slow)))
  (ok (eval-mark-expr :slow '(:slow)))
  (ng (eval-mark-expr :slow '(:fast)))
  (ok (eval-mark-expr '(not :slow) '(:fast)))
  (ok (eval-mark-expr '(or :slow :fast) '(:fast)))
  (ng (eval-mark-expr '(and :slow :fast) '(:slow)))
  (let ((*mark-expr* :slow))
    (ok (test-selected-by-marks-p 'marked-slow))
    (ng (test-selected-by-marks-p 'marked-fast)))
  (let ((*mark-expr* '(not :slow)))
    (ng (test-selected-by-marks-p 'marked-slow))
    (ok (test-selected-by-marks-p 'marked-fast))))

(deftest timeout-metadata
  (ok (= 30 (test-timeout 'marked-timeout))))

(deftest selected-tests-by-marks-and-shard
  (let* ((suite (package-suite *package*))
         (*mark-expr* :slow)
         (*shard* nil)
         (*shard-count* nil)
         (marked (selected-suite-tests suite)))
    (ok (member 'marked-slow marked))
    (ng (member 'marked-fast marked)))
  (let* ((suite (package-suite *package*))
         (*mark-expr* nil)
         (*shard* 0)
         (*shard-count* 2)
         (shard-0 (selected-suite-tests suite))
         (*shard* 1)
         (shard-1 (selected-suite-tests suite)))
    (ok (every (lambda (name)
                 (not (member name shard-1)))
               shard-0))
    (ok (plusp (length shard-0)))
    (ok (plusp (length shard-1)))))

(deftest timeout-records-failure
  (let ((name (intern (string (gensym "TO")) :rove/tests/marks))
        (*debug-on-error* nil))
    (eval `(deftest (,name :timeout 0.05)
             (sleep 1)))
    (unwind-protect
         (ng (run-test name :style :none))
      (remove-test name))))
