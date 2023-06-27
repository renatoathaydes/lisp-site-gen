(defpackage lisp-site-gen.integration-tests
  (:use :cl :fiveam))
(in-package :lisp-site-gen.integration-tests)

;; NOTE: To run this test file, execute `(asdf:test-system :lisp-site-gen)' in your Lisp.

(defmacro test-main (name description md-file expected)
  `(test ,name
     ,description
     (is (equal
          (with-output-to-string (str)
            (lisp-site-gen:run '(,md-file) str)
            str)
          ,expected))))


(test-main parse-example-md "can parse example.md"
           "tests/example.md"
           (concatenate 'string
                        "<H1>Header</H1>"
                        "<P></P><P>This is a paragraph.</P><P></P>"
                        (format nil "<QUOTATION> Quotes may~%contain multi-line.</QUOTATION>")
                        "<H2>Something.</H2>"
                        "<P></P><P><SPAN>This is <CODE>Lisp</CODE> code:</SPAN></P><P></P>"
                        "<PRE><CODE LANG=\"lisp\">"
                        (format nil "(defun looks-cool~%    (it-does))")
                        "</CODE></PRE>"))
