(defpackage lisp-site-gen
  (:use :cl :uiop :lisp-site-gen.md-parser :lisp-site-gen.html-gen))
(in-package lisp-site-gen)

(defun main ()
  (let ((args (command-line-arguments)))
    (if (= 1 (length args))
        (parse-markdown-file (first args) (html-generator *standard-output*))
        (error "one argument expected: the name of the markdown file to parse"))))
