(defpackage md-span-parser
  (:use :cl)
  (:export #:parse-md-span))

(in-package :md-span-parser)

(defun go-up-to (c sym1 sym2 line index rcv)
  (let* ((two-chars? (and
                      (< (1+ index) (length line))
                      (char= c (char line (1+ index)))))
         (adder (if two-chars? 2 1))
         (maybe-end (if two-chars?
                        (search (make-string 2 :initial-element c) line :start2 (+ index 2))
                        (position c line :start (1+ index)))))
    (if maybe-end
        (funcall rcv (list (if two-chars? sym2 sym1) (subseq line (+ index adder) maybe-end)))
        (funcall rcv (subseq line index)))
    (if maybe-end (+ maybe-end adder) (length line))))


(defun emit-then-go-to (c sym1 sym2 line prev-index index rcv)
  (when (< prev-index index)
    (funcall rcv (subseq line prev-index index)))
  (go-up-to c sym1 sym2 line index rcv))

(defun emit-link (line rcv start text-end parens-end)
  (let ((text (subseq line start text-end))
        (url (subseq line (+ 2 text-end) parens-end)))
    (funcall rcv (list :a text url))
    ;; return the next index for the parser to continue
    (1+ parens-end)))

(defun parse-link (line prev-index index rcv)
  (when (< prev-index index)
    (funcall rcv (subseq line prev-index index)))
  (let* ((index+1 (1+ index))
         (text-end (position #\] line :start index+1)))
    (when text-end
      (let* ((not-end? (< (1+ text-end) (length line)))
             (parens? (and not-end? (char= (char line (1+ text-end)) #\LEFT_PARENTHESIS))))
        (when parens?
          (let ((parens-end (position #\RIGHT_PARENTHESIS line :start text-end)))
            (when parens-end
              (emit-link line rcv index+1 text-end parens-end))))))))

(defun parse-line (line rcv)
  (let ((prev-index 0)
        (len (length line)))
    (loop for i from 0 below len do
      (let ((next-index (case (char line i)
                          (#\* (emit-then-go-to #\* :em :strong line prev-index i rcv))
                          (#\` (emit-then-go-to #\` :code :code line prev-index i rcv))
                          (#\[ (parse-link line prev-index i rcv)))))
        (when next-index
          (setf i next-index prev-index next-index)))
          finally (when (< prev-index len)
                    (funcall rcv (subseq line prev-index))))))

(defun singleton? (lst)
  (null (cdr lst)))

(defun parse-md-span (line)
  (let ((result nil))
    (parse-line line (lambda (x) (push x result)))
    (cond
      ((null result) (list :p ""))
      ((singleton? result) (list :p (car result)))
      (t (list :p (cons :span (nreverse result)))))))
