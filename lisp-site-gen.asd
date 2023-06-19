(defsystem "lisp-site-gen"
  :description "Common Lisp-based Static Site Generator"
  :version "0.1.0"
  :author "Renato Athaydes"
  :license "BSD"
  :pathname "src/"
  :depends-on ()
  :components ((:file "parse-md-span")
               (:file "parse" :depends-on ("parse-md-span"))
               (:static-file "LICENCE" :pathname #P"LICENCE"))
  :in-order-to ((test-op (test-op "lisp-site-gen/tests"))))

(defsystem "lisp-site-gen/executable"
  :build-operation program-op
  :pathname "bin"
  :build-pathname "lisp-md" ;; shell name
  :entry-point "lisp-site-gen::main" ;; thunk
  :depends-on ("lisp-site-gen")
  :components ((:file "main")))

(defsystem "lisp-site-gen/tests"
  :pathname "tests/"
  :depends-on ("lisp-site-gen" "rove")
  :components ((:file "parse-tests"))
  :perform (test-op (o c) (symbol-call :rove :run c)))
