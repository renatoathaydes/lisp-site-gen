(defpackage lisp-site-gen.html-code-style
  (:use :cl)
  (:import-from :cl-ppcre
   :scan :parse-string)
  (:export #:style-html #:style-code #:next-separator))

(in-package lisp-site-gen.html-code-style)

(defvar +spaces+ (parse-string "^\\s+"))

(defgeneric style-code (language text start)
  (:documentation
   "Get CSS styles for the text starting at `start`.
    The `language` is a symbol, which means it can be specialized on for each language.
    Return values `t`, the CSS style string, and the end-index of the text the style applies to.
    Return nil if the text should not be styled."))

(defgeneric next-separator (language text start)
  (:documentation
   "Find the index of the next separator in the text starting at `start`.
    When styling code, if `style-code` does not return a match, this method is called
    in order to find out the next character to continue from.
    The `language` is a symbol, which means it can be specialized on for each language."))

(defmethod style-code (language text start)
  "Default implementation, always returns nil."
  nil)

(defmethod next-separator (language text start)
  "Default implementation, returns the next whitespace."
  (scan +spaces+ text :start start))

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
    (multiple-value-bind (ok style next-index) (style-code lang text start)
      (let ((found-index
              (when ok
                (when (within-bounds next-index start end)
                  (funcall rcv (list :span :style style (subseq text start next-index)))
                  next-index))))
        (or found-index
            (emit-plain-text-up-to-separator))))))

(defun style-html (text lang rcv &optional (start 0))
  "Encode plain text into colorized HTML"
  (if (null lang)
      (funcall rcv (list (subseq text start)))
      (let ((lang-sym (or (and (symbolp lang) lang)
                          (intern (string-upcase lang)))))
        (loop with index = start and len = (length text)
              while (< index len)
              do (setf index (emit-styled-span text lang-sym rcv index len))))))
