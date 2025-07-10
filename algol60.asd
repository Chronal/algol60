;;;; algol60.asd
(asdf:defsystem algol60
  :description "An implementation of algol60"
  :author "Anuj <chronal@chronal.space>"
  :license  "Apache-2.0"
  :version "0.0.1"
  :serial t
  :depends-on (alexandria parse-float)
  :pathname "src"
  :components ((:file package)
               (:module lex
                :serial t
                :components ((:file package)
                             (:file utils)
                             (:file defs)
                             (:file lex)))))
