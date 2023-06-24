(defpackage lisp-site-gen.parser-tests
  (:use :cl :rove :lisp-site-gen.md-parser))
(in-package :lisp-site-gen.parser-tests)

;; NOTE: To run this test file, execute `(asdf:test-system :lisp-site-gen)' in your Lisp.

(defmacro test-md (name lines expected)
  `(testing ,name
    (let ((output nil))
      (parse-markdown-lines ,lines (lambda (o) (push o output)))
      (ok (equal (reverse output) ,expected)))))

(deftest parse-markdown-text

  (test-md "can parse empty line"
           '("")
           '((:p "")))

  (test-md "can parse plain text"
           '("simple line" "other text")
           '((:p "simple line") (:p "other text")))

  (test-md "can parse header 1"
           '("# hello")
           '((:h1 "hello")))

  (test-md "can parse header 2"
           '("## hello")
           '((:h2 "hello")))

  (test-md "can parse header 3"
           '("### header 3")
           '((:h3 "header 3")))

  (test-md "can parse header 7"
           '("####### 7")
           '((:h7 "7")))

  (test-md "can parse simple quote"
           '("> a quote")
           '((:quotation " a quote")))

  (test-md "can parse multi-line quote"
           '(">start" "end")
           `((:quotation ,(format nil "start~%end"))))

  (test-md "can parse text with emphasis"
           '("hello *world*!")
           '((:p (:span "hello " (:em "world") "!"))))

  (test-md "can parse text with strong"
           '("this is **very** important.")
           '((:p (:span "this is " (:strong "very") " important."))))

  (test-md "can parse text with inline code"
           '("`var` or ``var2``")
           '((:p (:span (:code "var") " or " (:code "var2")))))

  (test-md "can parse code block"
           '("hi" "```lisp" "(def main () (hello))" "(done)" "```")
           `((:p "hi") (:pre (:code :lang "lisp" ,(format nil "(def main () (hello))~%(done)")))))

)

(deftest parse-markdown-links

  (test-md "can parse simple link"
           '("go to [description](http://example.org/)")
           '((:p (:span "go to " (:a :href "http://example.org/" "description")))))

)
