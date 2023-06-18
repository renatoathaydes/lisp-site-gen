(defpackage lisp-site-gen
  (:use :cl :uiop :md-parser))
(in-package lisp-site-gen)

;; TODO send markdown to html generator
(defun md-receiver (md)
  (print md))

(defun main ()
  (let ((args (command-line-arguments)))
    (if (= 1 (length args))
        (md-parser:parse-markdown (first args) #'md-receiver)
        (error "one argument expected: the name of the markdown file to parse"))))
