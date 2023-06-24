(defpackage lisp-site-gen
  (:use :cl :uiop :md-parser :html-gen))
(in-package lisp-site-gen)

(defun main ()
  (let ((args (command-line-arguments)))
    (if (= 1 (length args))
        (md-parser:parse-markdown (first args) (html-generator *standard-output*))
        (error "one argument expected: the name of the markdown file to parse"))))
