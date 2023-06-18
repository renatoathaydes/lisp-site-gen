#!env /bin/bash

sbcl --non-interactive --eval '(asdf:test-system :lisp-site-gen)'
