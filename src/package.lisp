;;;; package.lisp

(defpackage algol60/lex
  (:use :cl)
  (:import-from :alexandria define-constant)
  (:import-from :parse-float parse-float)
  (:local-nicknames (:alex :alexandria))
  (:export tokenise))

(defpackage algol60/parser
  (:use :cl)
  (:import-from :alexandria define-constant)
  (:local-nicknames (:alex :alexandria))
  (:export parse))

(defpackage algol60
  (:use :cl)
  (:import-from algol60/lex tokenise)
  (:import-from algol60/parser parse))
