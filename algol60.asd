;;;; algol60.asd

(asdf:defsystem #:algol60
  :description "An implementation of algol60"
  :author "Anuj <chronal@chronal.space>"
  :license  "AGPL-3.0-or-later"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria #:clingon #:iterate)
  :components ((:file "package")
               (:file "algol60")
               (:file "lexer")
               (:file "parser")
               (:file "utils")))
