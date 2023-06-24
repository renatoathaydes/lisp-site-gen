(defpackage lisp-site-gen.integration-tests
  (:use :cl :rove))
(in-package :lisp-site-gen.integration-tests)

;; NOTE: To run this test file, execute `(asdf:test-system :lisp-site-gen)' in your Lisp.

(defmacro test-main (name md-file expected)
  `(testing ,name
     (ok (equal
          (with-output-to-string (str)
            (lisp-site-gen:run '(,md-file) str)
            str)
          ,expected))))

(deftest integration-test

  (test-main "can parse example.md"
             "tests/example.md"
             (concatenate 'string
                          "<H1>Header</H1>"
                          "<P></P><P>This is a paragraph.</P><P></P>"
                          (format nil "<QUOTATION> Quotes may~%contain multi-line.</QUOTATION>")
                          "<H2>Something.</H2>"))

)
