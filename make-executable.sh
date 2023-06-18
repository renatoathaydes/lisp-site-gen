#!env /bin/bash

COMPILE=${LISP:-sbcl}

$COMPILE --non-interactive --eval '(asdf:make :lisp-site-gen/executable)'
