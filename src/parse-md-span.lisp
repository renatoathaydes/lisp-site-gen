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

(defun parse-line (line rcv)
  (let ((prev-index 0)
        (len (length line)))
    (loop for i from 0 below len do
      (let ((next-index (case (char line i)
                          (#\* (emit-then-go-to #\* :em :strong line prev-index i rcv))
                          (#\` (emit-then-go-to #\` :code :code line prev-index i rcv)))))
        (when next-index
          (setf i next-index prev-index next-index)))
          finally (when (< prev-index len)
                    (funcall rcv (subseq line prev-index))))))

(defun parse-md-span (line)
  (let ((result nil))
    (parse-line line (lambda (x) (push x result)))
    (cons :span (nreverse result))))
