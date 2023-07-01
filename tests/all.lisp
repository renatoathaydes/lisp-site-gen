(defpackage lisp-site-gen.tests
  (:use :cl :fiveam)
  (:export :all-tests))

(in-package :lisp-site-gen.tests)

(def-suite all-tests :description "Top-level Test Suite including all tests.")

(defun run-all! ()
  (let ((ok (run! 'all-tests)))
    (unless ok (error "There were test failures."))))
