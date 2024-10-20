(in-package #:algol60)

(defvar *file-name* #p"./example-programs/e_approx.algol")

;; Use underscore as the strop char
(defparameter *strop-char* #\_)

;;;; Core class driving the lexing
;;;; Ideally later it can be configurable
;;;; to support different strop chars/parens/brackets
(defclass lexer ()
  ((line-num :initform 0)
   (column  :initform 0)
   (index :initform 0)
   (data :initarg :source
         :initform (error "Must source code to process"))
   file-len
   (finished :initform nil :reader lex-finished?)
   (tokens :initform (make-token-array))))

(defun make-lexer (source-code)
  (make-instance 'lexer :source source-code))

(defmethod initialize-instance :after ((lexer lexer) &key)
  (with-slots (data file-len) lexer
    (setf file-len (length data))))

(defmethod print-object ((L lexer ) stream)
  (print-unreadable-object (L stream :type t :identity t)
    (with-slots (line-num column index finished) L
        (format stream " ~a:~a, index ~a, finished? ~a" line-num column index finished))))

(defmethod reset-lexer ((lexer lexer))
  (with-slots (column index finished
               line-num tokens) lexer
    (setf line-num 0)
    (setf column 0)
    (setf index 0)
    (setf finished nil)
    (setf (fill-pointer tokens) 0)))

(defmethod lex ((L lexer))
  (with-slots (tokens index) L
    (loop for c = (current-char L)
          until (lex-finished? L) do
            (cond
              ((alpha? c) (read-variable L))
              ((comma? c) (push-token L 'comma))
              ((colon? c) (read-assign-op L))
              ((strop? c) (read-keyword L))
              ((semi-colon? c) (push-token L 'semi-colon)))
            (advance L)
          finally (return tokens))))

(defmethod read-assign-op ((lexer lexer))
  (with-slots (line-num column) lexer
    (cond
      ((equal-sign? (peek lexer)) (push-token lexer '(op assign)))
      (t (error "Expected '=' after the ':' ~a:~a~%" line-num column)))))

(defmethod peek((lexer lexer))
  (with-slots (data index) lexer
    (when (> (chars-left lexer :lookahead 1) 0)
      (char data (1+ index)))))

(defmethod push-token ((lexer lexer) tok)
  (with-slots (tokens) lexer
    (vector-push-extend tok tokens)))

(defmethod read-variable ((L lexer))
  (with-slots (data index) L
    (flet ((read-while-valid-name ()
             (loop for c = (peek L)
                   with start = index
                   while (or (alpha? c)
                             (digit? c)
                             (white-space? c)) do
                               (advance L)
                   finally
                      (return (list start (1+ index))))))
      (destructuring-bind (start end) (read-while-valid-name)
        (let ((ident (trim-ident (subseq data start end))))
          (push-token L `(ident ,ident)))))))

(defmethod read-keyword ((L lexer))
  (with-slots (data index line-num column) L
    (let* ((next (position *strop-char* data :start (1+ index)))
           (keyword-candidate (subseq data (1+ index) next))
           (iters (- next index)))
      (cond
        ((valid-stropped-keyword? keyword-candidate)
         (push-token L `(keyword ,(intern (string-upcase keyword-candidate))))
         (advance L iters))
        (t (error "Invalid keyword starting at ~a:~a" line-num column))))))

(defmethod advance ((lexer lexer) &optional (n 1))
  (dotimes (i n)
    (with-slots (index column
                 line-num finished) lexer
      (when (new-line? (current-char lexer))
        (incf line-num)
        (setf column 0))
      (cond
        ((> (chars-left lexer) 0)
         (incf index))
        (t
         (setf finished t))))))

(defmethod chars-left ((lexer lexer) &key (lookahead 0))
  (with-slots (index data) lexer
    (- (length data) (+ index lookahead 1))))

(defmethod current-char ((lexer lexer))
  (with-slots (data index) lexer
    (char data index)))

;;;; The following functions are just character predicates
;;;; for the lexer along with some utility functions
(defun alpha? (char)
  (alpha-char-p char))

;; [0-9]
(defun digit? (char)
  (digit-char-p char 10))

(defun paren? (char)
  (or (char= #\( char)
      (char= #\) char)))

(defun square-bracket? (char)
  (or (char= #\[ char)
      (char= #\] char)))

;; Maybe change this later to something nicer
(defun quote-bracket? (char)
  (or (char= #\{ char)
      (char= #\} char)))

(defun dot? (char)
  (char= char #\.))

(defun comma? (char)
  (char= char #\,))

(defun colon? (char)
  (char= char #\:))

(defun semi-colon? (char)
  (char= char #\;))

(defun strop? (c)
  (char= c *strop-char*))

(defun white-space? (c)
  (or (char= c #\Newline)
      (char= c #\Space)
      (char= c #\Tab)))

(defun equal-sign? (c)
  (char= c #\=))

(defun less-than-sign? (c)
  (char= c #\<))

(defun more-than-sign? (c)
  (char= c #\>))

(defun tilde? (c)
  (char= c #\~))

(defun new-line? (char)
  (char= char #\Newline))

(defun valid-stropped-keyword? (str)
  (labels
      ((equal-to-input? (keyword)
         (string-equal keyword str)))
    (some #'equal-to-input?
          '(
            ;; Booleans
            "true"
            "false"
            ;; Sequential operators
            "goto"
            "go to"
            "if"
            "then"
            "else"
            "for"
            "do"
            ;; Separators
            "step"
            "until"
            "while"
            "comment"
            ;; Bracket
            "begin"
            "end"
            ;; Declarator
            "own"
            "Boolean"
            "integer"
            "real"
            "array"
            "switch"
            "procedure"
            ;; Specificator
            "string"
            "label"
            "value"

            ;; Required for exponent syntax
            "10"))))

(defun trim-ident (ident)
  (string-trim '(#\NewLine
                 #\Space
                 #\Tab) ident))

;;; Types of Tokens
;;; '(ident ...)
;;; '(keyword ...)
;;;
;;;
;;; Punctuation
;;; , => 'comma
;;; ";" => 'semi-colon
;;; . => 'dot
;;;
;;; Operators
;;; + => 'add
;;; * => 'mult
;;; - => 'sub
;;; / => 'div
;;; ^ => 'pow
;;;
;;; & => 'and
;;; | => 'or
;;;
;;;
;;; => 'more-than
;;; >= => more-than-eq
;;;
;;; < => 'less-than
;;; <= => 'less-than-eq
;;;
;;; = => 'eq
;;; ~= => 'neq
;;; ~ => 'not
;;;
;;; Brackets
;;; 'open_paren
;;; 'close_paren
;;; 'open_string
;;; 'close_string
;;; 'open_block => _begin_
;;; 'close_block => _end_
;; [A-Za-z]

(defvar *lexer* (make-lexer (read-file-into-string *file-name*)))
