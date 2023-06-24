(defsystem "lisp-site-gen"
  :description "Common Lisp-based Static Site Generator"
  :version "0.1.0"
  :author "Renato Athaydes"
  :license "BSD"
  :version "0.1.0"
  :homepage "https://github.com/renatoathaydes/lisp-site-gen"
  :pathname "src/"
  :depends-on (:trivia)
  :components ((:file "parse-md-span")
               (:file "md-parser" :depends-on ("parse-md-span"))
               (:file "html-gen")
               (:static-file "LICENCE" :pathname #P"LICENCE"))
  :in-order-to ((test-op (test-op "lisp-site-gen/tests"))))

(defsystem "lisp-site-gen/executable"
  :build-operation program-op
  :pathname "bin"
  :build-pathname "lisp-site-gen" ;; shell name
  :entry-point "lisp-site-gen::main" ;; thunk
  :depends-on ("lisp-site-gen")
  :components ((:file "main")))

(defsystem "lisp-site-gen/tests"
  :pathname "tests/"
  :depends-on ("lisp-site-gen" "rove")
  :components ((:file "md-parser-tests"))
  :perform (test-op (o c) (symbol-call :rove :run c)))
