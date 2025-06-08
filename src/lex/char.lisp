(in-package :algol60/lex)

;;;; The following functions are just character predicates
;;; [a-zA-Z]
(defun alpha? (char)
  (alpha-char-p char))

;;; [0-9]
(defun digit? (c)
  (digit-char-p c 10))

(defun hex-digit? (c)
  (digit-char-p c 16))

(defun bin-dgit? (c)
  (digit-char-p c 2))

(defun open-paren? (c)
  (char= #\( c))

(defun close-paren? (c)
  (char= #\) c))

(defun open-subscript? (c)
  (char= #\[ c))

(defun close-subscript? (c)
  (char= #\] c))

(defun double-quote? (c)
  (char= #\" c))

(defun single-quote? (c)
  (char= #\' c))

(defun dot? (char)
  (char= char #\.))

(defun comma? (char)
  (char= char #\,))

(defun colon? (char)
  (char= char #\:))

(defun semi-colon? (char)
  (char= char #\;))

(defun white-space? (c)
  (or (char= c #\Newline)
      (char= c #\Space)
      (char= c #\Tab)))

(defun equal-sign? (c)
  (char= c #\=))

(defun plus? (c)
  (char= c #\+))

(defun slash? (c)
  (char= c #\/))

(defun backslash? (c)
  (char= c #\\))

(defun asterisk? (c)
  (char= c #\*))

(defun dash? (c)
  (char= c #\-))

(defun caret? (c)
  (char= c #\^))

(defun less-than-sign? (c)
  (char= c #\<))

(defun more-than-sign? (c)
  (char= c #\>))

(defun tilde? (c)
  (char= c #\~))

(defun new-line? (char)
  (char= char #\Newline))
