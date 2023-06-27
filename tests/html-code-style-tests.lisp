(defpackage lisp-site-gen.html-code-style.tests
  (:use :cl :fiveam)
  (:import-from :lisp-site-gen.html-code-style
   :style-html :style-code)
  (:import-from :cl-ppcre
   :scan))
(in-package :lisp-site-gen.html-code-style.tests)

(def-suite style-suite
  :description "Tests for the html-code-style package."
  :in lisp-site-gen.tests:all-tests)
(in-suite style-suite)

;; macro for creating tests easily

(defmacro test-style (name description text lang expected)
  `(test ,name
     ,description
     (let ((output nil))
       (style-html ,text ,lang (lambda (o) (push o output)))
       (is (equal ,expected (reverse output))))))

;; setup functions

(defmethod style-code ((language (eql 'mylang)) text start)
  "Define a test language so we can emit styled output"
  (let ((index (scan "\\s+" text)))
    (when index (values t "color: red" index))))

;; tests

(test-style plain-text-by-default "emits plain text by default"
            "hello world" nil
            '(("hello world")))

(test-style plain-text-if-unknown "emits plain text for unknown language"
            "hello world" nil
            '(("hello world")))

(test-style styled-text-if-known "emits styled text for known language"
            "hello world" 'mylang
            '((:span :style "color: red" "hello") (" world")))

;; helper functions

(defun test-style-suite ()
  "Helper function to run test suite"
  (run! 'style-suite))