(defpackage lisp-site-gen
  (:use
   :cl
   :uiop
   :lisp-site-gen.md-parser
   :lisp-site-gen.html-gen
   :lisp-site-gen.html-code-style)
  (:export #:run))
(in-package lisp-site-gen)

(defun run (args &optional (out *standard-output*))
  (if (= 1 (length args))
      (parse-markdown-file (first args) (map-code-style (html-generator out)))
      (error "one argument expected: the name of the markdown file to parse")))

(defun main ()
  (run (command-line-arguments)))
