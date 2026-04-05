(in-package #:algol60/lex)

(defstruct token
  type
  data)

(defun simple-token (sym)
  (make-token
   :type sym))

(defun simple-token? (token)
  (null (token-data token)))
