(defpackage md-span-parser
  (:use :cl)
  (:export #:parse-md-span))

(in-package :md-span-parser)

(defun go-up-to (c sym line index rcv)
  (let ((maybe-end (position c line :start (1+ index))))
    (if maybe-end
        (funcall rcv (list sym (subseq line (1+ index) maybe-end)))
        (funcall rcv (subseq line index)))
    (if maybe-end (1+ maybe-end) (length line))))


(defun emit-then-go-to (c sym line prev-index index rcv)
  (when (< prev-index index)
    (funcall rcv (subseq line prev-index index)))
  (go-up-to c sym line index rcv))

(defun parse-line (line rcv)
  (let ((prev-index 0))
   (loop for i from 0 below (length line) do
    (let ((next-index (case (char line i)
                        (#\* (emit-then-go-to #\* :em   line prev-index i rcv))
                        (#\` (emit-then-go-to #\` :code line prev-index i rcv)))))
      (when next-index
        (setf i next-index prev-index next-index)))
         finally (when (< prev-index (- (length line) 1))
                   (funcall rcv (subseq line prev-index))))))

(defun parse-md-span (line)
  (let ((result nil))
    (parse-line line (lambda (x) (push x result)))
    (cons :span (nreverse result))))
