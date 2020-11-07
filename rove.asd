(defsystem "rove"
  :class :package-inferred-system
  :version "0.9.6"
  :author "Eitaro Fukamachi"
  :license "BSD 3-Clause"
  :depends-on ("rove/main")
  :description "Yet another testing framework intended to be a successor of Prove"
  :in-order-to ((test-op (test-op "rove/tests"))))

(defsystem "rove/tests"
  :class :package-inferred-system
  :depends-on ("rove"
               "rove/tests")
  :perform (test-op (o c) (uiop:symbol-call :rove/tests :run)))
