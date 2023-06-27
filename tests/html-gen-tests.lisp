(defpackage lisp-site-gen.html-gen-tests
  (:use :cl :fiveam :lisp-site-gen.html-gen))
(in-package :lisp-site-gen.html-gen-tests)

(def-suite html-gen-suite
  :description "Tests for the html-gen package."
  :in lisp-site-gen.tests:all-tests)
(in-suite html-gen-suite)

(defmacro test-html-gen (name description input expected)
  (let ((result (gensym)))
    `(test ,name
       ,description
       (let ((,result (with-output-to-string (str)
                        (generate-html str ,input))))
         (is (equal ,result ,expected))))))


(test-html-gen empty-line "can generate empty line (empty list)"
               '()
               "")

(test-html-gen empty-line-empty-string "can generate empty line (list with empty string)"
               '((""))
               "")

(test-html-gen empty-paragraph "can generate empty paragraph"
               '((:p ""))
               "<P></P>")

(test-html-gen generate-paragraph "can generate paragraph"
               '((:p "hello world"))
               "<P>hello world</P>")

(test-html-gen generate-link "can generate link"
               '((:a :href "http://hello" "hi world"))
               "<A HREF=\"http://hello\">hi world</A>")

(test-html-gen generate-div "can generate div with contents"
               '((:div "hello"))
               "<DIV>hello</DIV>")

(test-html-gen generate-div-with-attrs "can generate div with contents and attributes"
               '((:div :class "minor" :id "hi" "hello"))
               "<DIV CLASS=\"minor\" ID=\"hi\">hello</DIV>")

(test-html-gen generate-span "can generate span with nested contents"
               '((:span "hello " (:em "world")))
               "<SPAN>hello <EM>world</EM></SPAN>")

(test-html-gen generate-span-attrs "can generate span with double-nested contents and attributes"
               '((:span (:em "world") (:foo (:bar :at "bcd" "contents"))))
               "<SPAN><EM>world</EM><FOO><BAR AT=\"bcd\">contents</BAR></FOO></SPAN>")


