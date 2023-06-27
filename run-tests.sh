#!env /bin/bash

COMPILE=${LISP:-sbcl}

$COMPILE --non-interactive --eval '
(handler-bind
  ((sb-ext:defconstant-uneql (lambda (c)
       (declare (ignore c))
       (invoke-restart (quote continue)))))
    (asdf:test-system :lisp-site-gen))'
