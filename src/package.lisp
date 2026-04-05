;;;; package.lisp

(defpackage algol60/lex
  (:use :cl :iterate)
  (:import-from :alexandria
                conjoin
                compose
                disjoin
                define-constant
                if-let
                when-let
                when-let*)
  (:import-from :parse-float parse-float)
  (:local-nicknames (:alex :alexandria))
  (:export tokenise-string tokenise-file))

(defpackage algol60/parser
  (:use :cl)
  (:import-from :alexandria define-constant)
  (:local-nicknames (:alex :alexandria))
  (:export parse))

(defpackage algol60
  (:use :cl)
  (:import-from algol60/lex tokenise-string)
  (:import-from algol60/parser parse))
