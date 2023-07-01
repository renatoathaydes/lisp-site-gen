#!env /bin/bash

COMPILE=${LISP:-sbcl}

$COMPILE --non-interactive --eval '
  (asdf:test-system :lisp-site-gen)
'
