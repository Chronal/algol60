(defpackage algol60/lex
  (:use :cl)
  (:import-from :alexandria define-constant)
  (:local-nicknames (:alex :alexandria))
  (:export tokenise-file tokenise-string))
