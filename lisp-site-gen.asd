(defsystem "lisp-site-gen"
  :description "Common Lisp-based Static Site Generator"
  :version "0.1.0"
  :author "Renato Athaydes"
  :license "BSD"
  :pathname "src/"
  :depends-on ("alexandria")
  :components ((:file "parse")
               (:static-file "LICENCE" :pathname #P"LICENCE"))
  :in-order-to ((test-op (test-op "lisp-site-gen/tests"))))

(defsystem "lisp-site-gen/tests"
  :pathname "tests/"
  :depends-on ("lisp-site-gen" "rove")
  :components ((:file "parse-tests"))
  :perform (test-op (o c) (symbol-call :rove :run c)))
