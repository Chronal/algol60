(defpackage algol60/lex
  (:use :cl)
  (:import-from :alexandria define-constant)
  (:import-from :parse-float parse-float)
  (:local-nicknames (:alex :alexandria))
  (:export tokenise-file tokenise-string))
