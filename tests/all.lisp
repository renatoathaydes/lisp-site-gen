(defpackage lisp-site-gen.tests
  (:use :cl :fiveam)
  (:export :all-tests))

(in-package :lisp-site-gen.tests)

(def-suite all-tests :description "Top-level Test Suite including all tests.")
