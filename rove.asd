(defsystem "rove"
  :class :package-inferred-system
  :version "0.10.0"
  :author "Eitaro Fukamachi"
  :license "BSD 3-Clause"
  :depends-on ("rove/main")
  :description "Yet another testing framework intended to be a successor of Prove"
  :in-order-to ((test-op (test-op "rove/tests"))))

(defsystem "rove/tests"
  :class :package-inferred-system
  :depends-on ("rove"
               "rove/tests/main")
  :perform (test-op (o c) (symbol-call :rove/tests/main :run-all-tests)))
