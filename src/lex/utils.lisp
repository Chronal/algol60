(in-package :algol60/lex)

;;; [a-zA-Z]
(defun alpha? (char)
  (alpha-char-p char))

;;; [0-9]
(defun digit? (c)
  (digit-char-p c 10))

;;; [a-zA-Z0-9]
(defun alnum? (c)
  (or (alpha? c) (digit? c)))

(defun sign? (c)
  (or (char= c #\+)
      (char= c #\-)))

(defun whitespace? (c)
  (or (char= c #\Newline)
      (char= c #\Space)
      (char= c #\Tab)))

;;; Global defs
(define-constant +keywords+
    '(
      ;; boolean constants
      true
      false

      ;; sequential operators
      goto
      if
      thne
      else
      for
      do
      
      ;; seperators
      step
      until
      while
      comment

      ;; brackets
      begin
      end

      ;; declarators
      own
      boolean
      integer
      real
      array
      switch
      procedure

      ;; specificator
      string
      label
      value) :test 'equal)

;;; Keyword
(defun make-keywords-ht ()
  (let ((keywords (make-hash-table :test 'equal)))
    (iter
      (for sym in +keywords+)
      (setf (gethash (string-downcase (string sym)) keywords) sym))
    keywords))

(defparameter *keywords* (make-keywords-ht))

(defun keyword? (ident)
  (multiple-value-bind (value present) (gethash ident *keywords*)
    (when present value)))

(define-constant +token-buf-init-len+ 256 :test '=)
