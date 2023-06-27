(defpackage lisp-site-gen.parser-tests
  (:use :cl :fiveam :lisp-site-gen.md-parser))
(in-package :lisp-site-gen.parser-tests)

(def-suite md-parser-suite
  :description "Tests for the md-parser package."
  :in lisp-site-gen.tests:all-tests)
(in-suite md-parser-suite)

(defmacro test-md (name description lines expected)
  `(test ,name
     ,description
     (let ((output nil))
       (parse-markdown-lines ,lines (lambda (o) (push o output)))
       (is (equal (reverse output) ,expected)))))


(test-md empty-line "can parse empty line"
         '("")
         '((:p "")))

(test-md plain-text "can parse plain text"
         '("simple line" "other text")
         '((:p "simple line") (:p "other text")))

(test-md header1 "can parse header 1"
         '("# hello")
         '((:h1 "hello")))

(test-md header2 "can parse header 2"
         '("## hello")
         '((:h2 "hello")))

(test-md header3 "can parse header 3"
         '("### header 3")
         '((:h3 "header 3")))

(test-md header7 "can parse header 7"
         '("####### 7")
         '((:h7 "7")))

(test-md simple-quote "can parse simple quote"
         '("> a quote")
         '((:quotation " a quote")))

(test-md multi-line-quote "can parse multi-line quote"
         '(">start" "end")
         `((:quotation ,(format nil "start~%end"))))

(test-md text-with-em "can parse text with emphasis"
         '("hello *world*!")
         '((:p (:span "hello " (:em "world") "!"))))

(test-md text-strong "can parse text with strong"
         '("this is **very** important.")
         '((:p (:span "this is " (:strong "very") " important."))))

(test-md inline-code "can parse text with inline code"
         '("`var` or ``var2``")
         '((:p (:span (:code "var") " or " (:code "var2")))))

(test-md code-block "can parse code block"
         '("hi" "```lisp" "(def main () (hello))" "(done)" "```")
         `((:p "hi") (:pre (:code :lang "lisp" ,(format nil "(def main () (hello))~%(done)")))))

(test-md simple-link "can parse simple link"
         '("go to [description](http://example.org/)")
         '((:p (:span "go to " (:a :href "http://example.org/" "description")))))
