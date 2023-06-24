(defpackage lisp-site-gen.md-parser
  (:use :cl :uiop :md-span-parser)
  (:export #:parse-markdown-file #:parse-markdown-lines))

(in-package :lisp-site-gen.md-parser)

(defstruct quote-state
  (string-lines nil :type list))

(defstruct code-block-state
  (language "" :type string)
  (string-lines nil :type list))

(defparameter *md-state* nil)

(defun parse-quote-line (line rcv)
  (flet ((terminate ()
           (funcall rcv (list
                         :quotation
                         (format nil "~{~a~^~%~}"
                                 (reverse (quote-state-string-lines *md-state*)))))
           (setq *md-state* nil)))
    (if (null line)
        (terminate)
        (if (string= line "")
            (terminate)
            (push line (quote-state-string-lines *md-state*))))))

(defun parse-code-block (line rcv)
  (flet ((terminate ()
           (funcall rcv (list
                         :pre
                         (list :code
                               :lang (code-block-state-language *md-state*)
                               (format nil "~{~a~^~%~}"
                                       (reverse (code-block-state-string-lines *md-state*))))))
           (setq *md-state* nil)))
    (if (null line)
        (terminate)
        (if (string-prefix-p "```" line)
            (terminate)
            (push line (code-block-state-string-lines *md-state*))))))

(defun parse-common-line (line rcv)
  (cond ((string-prefix-p "# " line) (funcall rcv (list :h1 (subseq line 2))))
        ((string-prefix-p "## " line) (funcall rcv (list :h2 (subseq line 3))))
        ((string-prefix-p "### " line) (funcall rcv (list :h3 (subseq line 4))))
        ((string-prefix-p "#### " line) (funcall rcv (list :h4 (subseq line 5))))
        ((string-prefix-p "##### " line) (funcall rcv (list :h5 (subseq line 6))))
        ((string-prefix-p "###### " line) (funcall rcv (list :h6 (subseq line 7))))
        ((string-prefix-p "####### " line) (funcall rcv (list :h7 (subseq line 8))))
        ((string-prefix-p "```" line) (progn
                                        (setq *md-state* (make-code-block-state
                                                          :language (string-trim " " (subseq line 3))))))
        ((string-prefix-p ">" line) (progn
                                      (setq *md-state* (make-quote-state))
                                      (parse-quote-line (subseq line 1) rcv)))
        (t (funcall rcv (parse-md-span line)))))

(defun parse-next-line (line rcv)
  (ecase (type-of *md-state*)
    (null (parse-common-line line rcv))
    (quote-state (parse-quote-line line rcv))
    (code-block-state (parse-code-block line rcv))))

(defun complete-parsing (rcv)
  (ecase (type-of *md-state*)
    (null nil)
    (quote-state (parse-quote-line nil rcv))))

(declaim (ftype (function (list (function (list)))) parse-markdown-lines))
(defun parse-markdown-lines (lines rcv)
  "Parse the markdown lines provided, emitting the output to the rcv function.
   The single argument..."
  (let ((*md-state* nil))
    (loop for line in lines do
      (parse-next-line line rcv))
    (complete-parsing rcv)))

(declaim (ftype (function (string (function (list)))) parse-markdown-file))
(defun parse-markdown-file (filename rcv)
  "Pasrse the given markdown file, emitting the output to the rcv function."
  (with-open-file (stream filename :if-does-not-exist nil)
    ;; reset md-state every time a new file starts
    (let ((*md-state* nil))
      (if stream
          (loop for line = (read-line stream nil) while line do
            (parse-next-line line rcv))
          (error "File does not exist"))
      (complete-parsing rcv))))
