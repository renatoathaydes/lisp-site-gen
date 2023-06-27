(defsystem "lisp-site-gen"
  :description "Common Lisp-based Static Site Generator"
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :version "0.1.0"
  :author "Renato Athaydes"
  :license "BSD"
  :version "0.1.0"
  :homepage "https://github.com/renatoathaydes/lisp-site-gen"
  :pathname "src/"
  :depends-on (:trivia :cl-ppcre)
  :components ((:file "parse-md-span")
               (:file "md-parser" :depends-on ("parse-md-span"))
               (:file "html-code-style")
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
  :depends-on ("lisp-site-gen"
               "lisp-site-gen/executable"
               ;; "lisp-site-gen/language-styles"
               "fiveam")
  :components ((:file "all")
               (:file "md-parser-tests" :depends-on ("all"))
               (:file "html-gen-tests" :depends-on ("all"))
               (:file "html-code-style-tests" :depends-on ("all"))
               (:file "integration" :depends-on ("all")))
  :perform (test-op (o c)
                    (symbol-call :fiveam :run!
                                 (find-symbol* :all-tests :lisp-site-gen.tests))))
