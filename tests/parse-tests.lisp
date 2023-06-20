(defpackage lisp-site-gen-test
  (:use :cl :rove :md-parser))
(in-package :lisp-site-gen-test)

;; NOTE: To run this test file, execute `(asdf:test-system :lisp-site-gen)' in your Lisp.

(deftest parse-markdown-text

  (testing "can parse plain text"
    (let ((output nil))
      (md-parser:parse-markdown-lines '("simple line" "other text") (lambda (o) (push o output)))
      (ok (equal (reverse output) '((:span "simple line") (:span "other text"))))))

  (testing "can parse header 1"
    (let ((output nil))
      (md-parser:parse-markdown-lines '("# hello") (lambda (o) (push o output)))
      (ok (equal (reverse output) '((:h1 "hello"))))))

  (testing "can parse header 2"
    (let ((output nil))
      (md-parser:parse-markdown-lines '("## hello") (lambda (o) (push o output)))
      (ok (equal (reverse output) '((:h2 "hello"))))))

  (testing "can parse header 3"
    (let ((output nil))
      (md-parser:parse-markdown-lines '("### header 3") (lambda (o) (push o output)))
      (ok (equal (reverse output) '((:h3 "header 3"))))))

  (testing "can parse header 7"
    (let ((output nil))
      (md-parser:parse-markdown-lines '("####### 7") (lambda (o) (push o output)))
      (ok (equal (reverse output) '((:h7 "7"))))))

  (testing "can parse simple quote"
    (let ((output nil))
      (md-parser:parse-markdown-lines '("> a quote") (lambda (o) (push o output)))
      (ok (equal (reverse output) '((:quotation " a quote"))))))

  (testing "can parse multi-line quote"
    (let ((output nil))
      (md-parser:parse-markdown-lines '(">start" "end") (lambda (o) (push o output)))
      (ok (equal (reverse output) `((:quotation ,(format nil "start~%end")))))))

  (testing "can parse text with emphasis"
    (let ((output nil))
      (md-parser:parse-markdown-lines '("hello *world*!") (lambda (o) (push o output)))
      (ok (equal (reverse output) '((:span "hello " (:em "world") "!"))))))

  (testing "can parse text with strong"
    (let ((output nil))
      (md-parser:parse-markdown-lines '("this is **very** important.") (lambda (o) (push o output)))
      (ok (equal (reverse output) '((:span "this is " (:strong "very") " important."))))))

  (testing "can parse text with inline code"
    (let ((output nil))
      (md-parser:parse-markdown-lines '("`var` or ``var2``") (lambda (o) (push o output)))
      (ok (equal (reverse output) '((:span (:code "var") " or " (:code "var2")))))))


)
