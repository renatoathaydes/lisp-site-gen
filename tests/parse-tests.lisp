(defpackage lisp-site-gen-test
  (:use :cl :rove :md-parser))
(in-package :lisp-site-gen-test)

;; NOTE: To run this test file, execute `(asdf:test-system :lisp-site-gen)' in your Lisp.

(defmacro test-md (name lines expected)
  `(testing ,name
    (let ((output nil))
      (md-parser:parse-markdown-lines ,lines (lambda (o) (push o output)))
      (ok (equal (reverse output) ,expected)))))

(deftest parse-markdown-text

  (test-md "can parse plain text"
           '("simple line" "other text")
           '((:span "simple line") (:span "other text")))

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
           '((:span "hello " (:em "world") "!")))

  (test-md "can parse text with strong"
           '("this is **very** important.")
           '((:span "this is " (:strong "very") " important.")))

  (test-md "can parse text with inline code"
           '("`var` or ``var2``")
           '((:span (:code "var") " or " (:code "var2"))))


)
