(in-package #:cl-user)

#-asdf3.1 (error "Rove requires ASDF 3.1")
(asdf:defsystem #:rove
  :class :package-inferred-system
  :version "0.1"
  :author "Eitaro Fukamachi"
  :license "BSD 3-Clause"
  :depends-on ("rove/main")
  :description "Small testing framework")
