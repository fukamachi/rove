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

(defun system-packages-finds-a-package-recorded-under-its-fasl ()
  ;; What RESOLVE-FILE records when it cannot map a fasl back to its source
  ;; file: the fasl's own pathname with the source type.
  (let* ((component (first (asdf:component-children
                            (asdf:find-system "rove/tests/marks"))))
         (fasl (first (asdf:output-files (asdf:make-operation 'asdf:compile-op)
                                         component)))
         (package (find-package :rove/tests/marks))
         (rove/core/suite/file::*file-package* (make-hash-table :test 'equal)))
    (setf (rove/core/suite/file:file-package (uiop:lispize-pathname fasl))
          package)
    (assert (equal (list package)
                   (rove/core/suite/file::system-packages
                    (asdf:find-system "rove/tests"))))))

(defun run-all-tests ()
  (testing-returns-the-body-result)
  (system-packages-finds-a-package-recorded-under-its-fasl)
  (assert (rove:run :rove/tests/marks)))
