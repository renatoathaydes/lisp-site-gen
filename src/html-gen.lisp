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

(defmacro add-html-tag-contents-recur (out rest &body body)
  "Recurse into add-html-tag-contents after inserting whitespace
   and running the given body."
  `(progn
     (add-space ,out)
     ,@body
     (add-html-tag-contents ,out ,rest)))

(defun add-html-tag-contents (out lst)
  (match lst
    ;; the empty list
    ((list) (write-string ">" out))
    ;; case like (:a :b ...)
    ((guard (list* a b rest)
            (and (keywordp a) (keywordp b)))
     (add-html-tag-contents-recur out (cons b rest)
       (add-attribute out a)))
    ;; case like (:a "b" ...)
    ((guard (list* a b rest)
            (and (keywordp a) (stringp b)))
     (add-html-tag-contents-recur out rest
       (add-attribute out a b)))
    ;; case like (a (...) ...), a becomes nested content
    ((guard (list* a b rest)
            (listp b))
     (write-string ">" out)
     (convert-to-html out a)
     (list-to-html out b)
     (convert-to-html out rest))
    ;; any other case, everything becomes nested content
    ((list* item)
     (write-string ">" out)
     (convert-to-html out item))))

(defun html-tag (out tag rest)
  (format out "<~a" tag)
  (add-html-tag-contents out rest)
  (format out "</~a>" tag))

(declaim (ftype (function (stream list)) list-to-html))
(defun list-to-html (out lst)
  (match lst
    ((list* head tail)
     (match head
       ((or (satisfies keywordp) (satisfies symbolp))
        (html-tag out head tail))
       ((satisfies consp)
        (list-to-html out head)
        (convert-to-html out tail))
       (otherwise (format out "~{~a~^ ~}" lst))))))

(declaim (ftype (function (stream (or keyword string list))) convert-to-html))
(defun convert-to-html (out html)
  (cond
    ((keywordp html) (html-tag out html nil))
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
