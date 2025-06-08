;;;; algol60.asd
(asdf:defsystem #:algol60
  :description "An implementation of algol60"
  :author "Anuj <chronal@chronal.space>"
  :license  "AGPL-3.0-or-later"
  :version "0.0.1"
  :serial t
  :depends-on (alexandria               ;;External Deps
               clingon
               iterate
               fset
               parse-float)

  :pathname "src"
  :components ((:file "package")
               (:module "lex"
                :serial t
                :components ((:file package)
                             (:file char)
                             (:file lex)))))
