(in-package algol60/lex)

(defparameter *keywords* (make-keywords-ht))
(defvar *strop-char* #\')
(defvar *stropped-keywords* nil)

(define-constant +token-buf-init-len+ 256 :test '=)
