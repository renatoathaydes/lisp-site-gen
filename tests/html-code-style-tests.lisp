(defpackage lisp-site-gen.html-code-style.tests
  (:use :cl :fiveam)
  (:import-from :lisp-site-gen.html-code-style
   :style-html :style-code :*css-class-transform*)
  (:import-from :cl-ppcre
   :scan))
(in-package :lisp-site-gen.html-code-style.tests)

(def-suite style-suite
  :description "Tests for the html-code-style package."
  :in lisp-site-gen.tests:all-tests)
(in-suite style-suite)

;; macro for creating tests easily

(defmacro test-style (name description text lang expected &key (let-vars '()))
  `(test ,name
     ,description
     (let ((output nil) ,@let-vars)
       (style-html ,text ,lang (lambda (o) (push o output)))
       (is (equal ,expected (reverse output))))))

;; setup functions

(defmethod style-code ((language (eql 'mylang)) text start)
  "Define a test language so we can emit styled output.
   This will style red any text preceeding whitespaces."
  (let ((index (scan "\\s+" text :start start)))
    (when index (values t "red" index))))

;; tests

(test-style plain-text-by-default "emits plain text by default"
            "hello world" nil
            '(("hello world")))

(test-style plain-text-if-unknown "emits plain text for unknown language"
            "hello world" nil
            '(("hello world")))

(test-style styled-text-if-known "emits styled text for known language"
            "hello " 'mylang
            '((:span :class "hljs-red" "hello")
              (" ")))

(test-style styled-text-if-known2 "emits styled text for known language (longer example)"
            "hello world foo(bar)" 'mylang
            '((:span :class "hljs-red" "hello")
              (" ")
              (:span :class "hljs-red" "world")
              (" ")
              ("foo(bar)")))

(test-style styled-text-if-known-custom-css-transform
            "emits styled text for known language (custom CSS transform)"
            "begin do done" 'mylang
            '((:span :class "red" "begin")
              (" ")
              (:span :class "red" "do")
              (" ")
              ("done"))
            :let-vars ((*css-class-transform* #'(lambda (c) c))))

;; helper functions

(defun test-style-suite ()
  "Helper function to run test suite"
  (run! 'style-suite))
