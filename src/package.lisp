;;;; package.lisp
(defpackage algol60
  (:use :cl)
  (:import-from algol60/lex tokenise-string)
  (:import-from algol60/parser parse))
