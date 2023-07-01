(defpackage lisp-site-gen.html-code-style.lisp
  (:use :cl)
  (:import-from :cl-ppcre
   :scan :parse-string)
  (:import-from :lisp-site-gen.html-code-style
   :style-html :html-style-make :style-code :next-separator)
  (:import-from :uiop
   :string-prefix-p))

(in-package lisp-site-gen.html-code-style.lisp)

(defparameter *lisp-terminator* "([`,\"'\\(\\)]|\\s+)")
(defparameter *lisp-def-keyword* (parse-string (concatenate 'string "^(defun|defparameter|defvar)" *lisp-terminator*)))
(defparameter *lisp-number* (parse-string (concatenate 'string "^(\\+|-)?[0-9]+((/[0-9]+)|(.[0-9]+))?" *lisp-terminator*)))
(defparameter *lisp-binary* (parse-string (concatenate 'string "^#b[0-1]+" *lisp-terminator*)))
(defparameter *lisp-number-radix* (parse-string (concatenate 'string "^#(x|[\\d{1,2}]r)[0-9a-zA-Z]+" *lisp-terminator*)))
(defparameter *lisp-literal* (parse-string (concatenate 'string "^(nil|t|#\[a-z]+)" *lisp-terminator*)))
(defparameter *lisp-separator* (parse-string *lisp-terminator*))

;;; Helper functions

(defun matches-string (text start)
  (when (char= (char text start) #\")
    (let ((escape nil))
      (loop for index from (1+ start) below (length text)
            ; terminate the string when a non-escaped double-quote is found
            do (when (and (not escape)
                          (char= (char text index) #\"))
                 (return (1+ index)))
               (setf escape (char= (char text index) #\BACKSLASH))))))

;;; html-code-style protocol implementation for LISP.

(defclass lisp-html-style ()
  ((next-title :initform nil
               :type (or null string))))

(defmethod html-style-make ((language (eql :LISP)))
  (make-instance 'lisp-html-style))

(defmethod style-code ((language lisp-html-style) text start)
  (with-slots (next-title) language
    (when next-title
      (let ((the-next-title next-title)
            (end (next-separator language text start)))
        (setf next-title nil)
        (return-from style-code (values t the-next-title end)))))
  (let ((index (matches-string text start)))
    (when index (return-from style-code (values t "string" index))))
  (multiple-value-bind (begin end)
      (scan *lisp-def-keyword* text :start start)
    (when begin
      ; the next word will be a title
      (setf (slot-value language 'next-title) "title")
      (return-from style-code (values t "keyword" end))))
  (multiple-value-bind (begin end)
      (scan *lisp-number* text :start start)
    (when begin (return-from style-code (values t "number" end))))
  (multiple-value-bind (begin end)
      (scan *lisp-number-radix* text :start start)
    (when begin (return-from style-code (values t "number" end))))
  (multiple-value-bind (begin end)
      (scan *lisp-binary* text :start start)
    (when begin (return-from style-code (values t "number" end))))
  (multiple-value-bind (begin end)
      (scan *lisp-literal* text :start start)
    (when begin (return-from style-code (values t "literal" end)))))

(defmethod next-separator ((language lisp-html-style) text start)
  (multiple-value-bind (start end) (scan *lisp-separator* text :start start)
    (and start end)))
