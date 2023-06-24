(defpackage lisp-site-gen.html-gen
  (:use :cl)
  (:import-from :trivia
   :match :guard)
  (:export #:html-generator #:generate-html))

(in-package lisp-site-gen.html-gen)

(defun add-attribute (out name &optional value)
  (if value
      (format out "~a=\"~a\"" name value)
      (format out "~a" name)))

(defun add-space (out)
  (write-string " " out))

(defmacro add-html-tag-contents-recur (out rest first? &body body)
  `(progn
     (unless ,first? (add-space ,out))
     ,@body
     (add-html-tag-contents ,out ,rest nil)))

(defun add-html-tag-contents (out lst &optional (first? t))
  (match lst
    ((list) (write-string ">" out))
    ((list content)
     (if (keywordp content)
         (add-html-tag-contents-recur out nil nil
           (add-attribute out content))
         (format out ">~a" content)))
    ((guard (list* a b rest)
            (keywordp a))
     (add-html-tag-contents-recur out rest first?
       (add-attribute out a b)))
    ((list* item rest)
     (add-html-tag-contents-recur out rest first?
       (format out "~a" item)))
    (otherwise (format out "~a" lst))))

(defun html-tag (out tag rest)
  (format out "<~a" tag)
  (add-html-tag-contents out rest)
  (format out "</~a>" tag))

(declaim (ftype (function (stream list)) list-to-html))
(defun list-to-html (out lst)
  (match lst
    ((list* head tail)
     (match head
       ((or (satisfies keywordp) (satisfies symbolp) (satisfies stringp))
        (html-tag out head tail))
       ((satisfies consp)
        (list-to-html out head)
        (convert-to-html out tail))
       (otherwise (format out "~{~a ~}" lst))))))

(declaim (ftype (function (stream (or string list))) convert-to-html))
(defun convert-to-html (out html)
  (cond
    ((stringp html) (write-string html out))
    ((consp html) (list-to-html out html))
    ((null html) nil)))

(declaim (ftype (function (stream list)) generate-html))
(defun generate-html (out html-sexp)
  (dolist (item html-sexp)
    (convert-to-html out item)))

(declaim (ftype (function (stream) (function (list))) html-generator))
(defun html-generator (out)
  (lambda (html)
    (generate-html out html)))
