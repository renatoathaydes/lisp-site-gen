(defpackage lisp-site-gen.html-gen-tests
  (:use :cl :rove :lisp-site-gen.html-gen))
(in-package :lisp-site-gen.html-gen-tests)

;; NOTE: To run this test file, execute `(asdf:test-system :lisp-site-gen)' in your Lisp.

(defmacro test-html-gen (name input expected)
  (let ((result (gensym)))
    `(testing ,name
       (let ((,result (with-output-to-string (str)
                       (generate-html str ,input))))
         (ok (equal ,result ,expected))))))

(deftest html-gen-test

  (test-html-gen "can generate empty line (empty list)"
                 '()
                 "")

  (test-html-gen "can generate empty line (list with empty string)"
                 '((""))
                 "")

  (test-html-gen "can generate empty paragraph"
                 '((:p ""))
                 "<P></P>")

  (test-html-gen "can generate paragraph"
                 '((:p "hello world"))
                 "<P>hello world</P>")

  (test-html-gen "can generate link"
                 '((:a :href "http://hello" "hi world"))
                 "<A HREF=\"http://hello\">hi world</A>")

  (test-html-gen "can generate div with contents"
                 '((:div "hello"))
                 "<DIV>hello</DIV>")

  (test-html-gen "can generate div with contents and attributes"
                 '((:div :class "minor" :id "hi" "hello"))
                 "<DIV CLASS=\"minor\" ID=\"hi\">hello</DIV>")

  (test-html-gen "can generate span with nested contents"
                 '((:span "hello " (:em "world")))
                 "<SPAN>hello <EM>world</EM></SPAN>")

  (test-html-gen "can generate span with double-nested contents and attributes"
                 '((:span (:em "world") (:foo (:bar :at "bcd" "contents"))))
                 "<SPAN><EM>world</EM><FOO><BAR AT=\"bcd\">contents</BAR></FOO></SPAN>")

)
