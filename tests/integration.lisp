(defpackage lisp-site-gen.integration-tests
  (:use :cl :fiveam))
(in-package :lisp-site-gen.integration-tests)

(def-suite integration-suite
  :description "Tests for the executable system (integration tests)."
  :in lisp-site-gen.tests:all-tests)
(in-suite integration-suite)

(defmacro test-main (name description md-file expected)
  `(test ,name
     ,description
     (is (equal
          ,expected
          (with-output-to-string (str)
            (lisp-site-gen:run '(,md-file) str)
            str)))))

;;; Tests

(test-main parse-example-md "can parse example.md"
           "tests/example.md"
           (concatenate 'string
                        "<H1>Header</H1>"
                        "<P></P><P>This is a paragraph.</P><P></P>"
                        (format nil "<QUOTATION> Quotes may~%contain multi-line.</QUOTATION>")
                        "<H2>Something.</H2>"
                        "<P></P><P><SPAN>This is <CODE>Lisp</CODE> code:</SPAN></P><P></P>"
                        "<PRE CLASS=\"hljs\"><CODE LANG=\"lisp\">"
                        "(<SPAN CLASS=\"hljs-keyword\">defun </SPAN>"
                        (format nil "<SPAN CLASS=\"hljs-title\">looks-cool </SPAN>()~%")
                        "    (it-does <SPAN CLASS=\"hljs-string\">\"definitely cool!\"</SPAN>))"
                        "</CODE></PRE>"))
