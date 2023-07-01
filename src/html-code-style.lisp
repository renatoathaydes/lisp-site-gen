(defpackage lisp-site-gen.html-code-style
  (:use :cl)
  (:import-from :cl-ppcre
   :scan :parse-string)
  (:import-from :trivia
   :match :guard)
  (:export #:style-html #:style-code #:next-separator
           #:*css-class-transform* #:map-code-style))

(in-package lisp-site-gen.html-code-style)

;;; CSS classes based on https://highlightjs.readthedocs.io/en/latest/css-classes-reference.html#stylable-scopes

(defvar +spaces+ (parse-string "\\s+"))

(defparameter *css-class-transform* (lambda (c) (format nil "hljs-~a" c))
  "Function to apply to the CSS classes returned by the `style-code` methods.
   The default function prefixes the classes with `hljs-`, making it possible to use
   highlight.js themes.")

(defun make-keyword (name)
  (if (symbolp name)
      name
      (values (intern (string-upcase name) "KEYWORD"))))

(defgeneric style-code (language text start)
  (:documentation
   "Get CSS styles for the text starting at `start`.
    The `language` is a keyword, which means it can be specialized on for each language.
    Return values `t`, the CSS classes as a string, and the end-index of the text the style applies to.
    Return nil if the text should not be styled."))

(defgeneric next-separator (language text start)
  (:documentation
   "Find the index of the next separator in the text starting at `start`.
    When styling code, if `style-code` does not return a match, this method is called
    in order to find out the next character to continue from.
    The `language` is a keyword, which means it can be specialized on for each language."))

(defmethod style-code (language text start)
  "Default implementation, always returns nil.
   To specialize this method, use EQL as follows:
     `(defmethod style-code ((language (eql :LANG-NAME) text start)) ...)`"
  nil)

(defmethod next-separator (language text start)
  "Default implementation, returns the next whitespace.
   To specialize this method, use EQL as follows:
     `(defmethod next-separator ((language (eql :LANG-NAME) text start)) ...)`"
  (multiple-value-bind (first last)
      (scan +spaces+ text :start start)
    (and first last)))

(declaim (ftype (function (fixnum fixnum fixnum) boolean) within-bounds))
(defun within-bounds (index start end)
  (and (> index start) (<= index end)))

(declaim (ftype (function (string symbol (function (list)) fixnum fixnum) fixnum) emit-styled-span))
(defun emit-styled-span (text lang rcv start end)
  "Emit the styled span if a style is found, or plain-text up to the next separator otherwise.
   Return the next index."
  (flet ((emit-plain-text-up-to-separator ()
           (let* ((next-index (next-separator lang text start))
                  (found-index
                    (if (and next-index (within-bounds next-index start end))
                        next-index
                        end)))
             (funcall rcv (list (subseq text start found-index)))
             found-index)))
    (multiple-value-bind (ok css-class next-index) (style-code lang text start)
      (let ((found-index
              (when ok
                (when (within-bounds next-index start end)
                  (funcall rcv (list :span
                                     :class (funcall *css-class-transform* css-class)
                                     (subseq text start next-index)))
                  next-index))))
        (or found-index
            (emit-plain-text-up-to-separator))))))

(declaim (ftype (function (string (or string symbol) (function (list)) &optional fixnum)) style-html))
(defun style-html (text lang rcv &optional (start 0))
  "Encode plain text into colorized HTML.
   Lists of s-expressions representing HTML are emitted by invoking the given `rcv` (receiver)
   function.
   The `style-code` generic method is used to style each token. When `style-code` identifies a token,
   it returns not only its style class, but also its end index, so that the lexer may continue from that
   point. If `style-code` returns `nil`, the `next-separator` generic method is used to find the next
   point from which to look for the next token.
   To implement support for a new language, specialize these two generic methods as appropriate."
  (if (null lang)
      (funcall rcv (list (subseq text start)))
      (let ((lang-sym (or (and (symbolp lang) lang)
                          (make-keyword lang))))
        (loop with index = start and len = (length text)
              while (< index len)
              do (setf index (emit-styled-span text lang-sym rcv index len))))))

(defun map-code-style (rcv)
  "Wraps a HTML receiving function so that any code block with a specified language
   may be styled by calling `style-html`.
   Any other HTML s-expressions are passed to the `rcv` function unmodified."
  (lambda (html)
    (match html
      ((guard (list :pre (list :code :lang lang code))
              (stringp code))
       (let ((code-block nil))
         (flet ((accumulate (h)
                  (push h code-block)))
           (style-html code lang #'accumulate)
           (funcall rcv (list :pre (list :code :lang lang (nreverse code-block)))))))
      (otherwise (funcall rcv html)))))
