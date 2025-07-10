(in-package :algol60/lex)

(defun strop? (c)
  (char= c *strop-char*))

;;; [a-zA-Z]
(defun alpha? (char)
  (alpha-char-p char))

;;; [0-9]
(defun digit? (c)
  (digit-char-p c 10))

;;; [a-zA-Z0-9]
(defun alnum? (c)
  (or (alpha? c) (digit? c)))

;;; Keyword
(defun make-keywords-ht ()
  (let ((keywords (make-hash-table :test 'equal)))
    ;; boolean constants
    (setf (gethash "true" keywords) 'true)
    (setf (gethash "false" keywords) 'false)

    ;; sequential operators
    (setf (gethash "goto" keywords) 'goto)
    (setf (gethash "if" keywords) 'if)
    (setf (gethash "then" keywords) 'then)
    (setf (gethash "else" keywords) 'else)
    (setf (gethash "for" keywords) 'for)
    (setf (gethash "do" keywords) 'do)

    ;; seperators
    (setf (gethash "step" keywords) 'step)
    (setf (gethash "until" keywords) 'until)
    (setf (gethash "while" keywords) 'while)
    (setf (gethash "comment" keywords) 'comment)

    ;; brackets
    (setf (gethash "begin" keywords) 'begin)
    (setf (gethash "end" keywords) 'end)

    ;; declarators
    (setf (gethash "own" keywords) 'own)
    (setf (gethash "Boolean" keywords) 'Boolean)
    (setf (gethash "integer" keywords) 'integer)
    (setf (gethash "real" keywords) 'real)
    (setf (gethash "array" keywords) 'array)
    (setf (gethash "switch" keywords) 'switch)
    (setf (gethash "procedure" keywords) 'procedure)

    ;; specificator
    (setf (gethash "string" keywords) 'string)
    (setf (gethash "label" keywords) 'label)
    (setf (gethash "value" keywords) 'value)

    keywords))

(defun keyword? (ident)
  (multiple-value-bind (value present) (gethash ident *keywords*)
    (if present
        value
        nil)))
