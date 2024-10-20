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

(defmacro define-lex-method (name args &body body)
  `(defmethod ,name ((,(first args) lexer) ,@(rest args))
     (with-slots (column data index file-len finished line-num tokens)
         ,(first args)
       ,@body)))

(define-lex-method reset-lexer (lexer)
  (setf line-num 0)
  (setf column 0)
  (setf index 0)
  (setf finished nil)
  (setf (fill-pointer tokens) 0))

(define-lex-method lex (L)
  (loop for c = (current-char L)
        until (lex-finished? L) do
          (cond
            ;; long/complex tokens
            ((alpha? c) (read-variable L))
            ((digit? c) (read-number L))
            ((colon? c) (read-assign-op L))
            ((strop? c) (read-keyword L))


            ;; two char tokens


            ;; single char tokens
            ;; brackets
            ((open-paren? c) (push-token l 'open-paren))
            ((close-paren? c) (push-token l 'close-paren))

            ((open-subscript? c) (push-token l 'open-subscript))
            ((close-subscript? c) (push-token l 'close-subscript))
            ;; operators
            ((plus? c) (push-token L '(op add)))
            ((dash? c) (push-token L '(op sub)))
            ((asterisk? c) (push-token L '(op mul)))
            ((slash? c) (push-token L '(op div)))
            ((caret? c) (push-token L '(op pow)))

            ;; separators
            ((comma? c) (push-token L 'comma))
            ((semi-colon? c) (push-token L 'semi-colon)))
          (advance L)
        finally (return tokens)))

(define-lex-method read-assign-op (lexer)
  (cond
    ((equal-sign? (peek lexer)) (push-token lexer '(op assign)))
    (t (error "Expected '=' after the ':' ~a:~a~%" line-num column))))

(define-lex-method peek (lexer)
  (when (> (chars-left lexer :lookahead 1) 0)
    (char data (1+ index))))

(define-lex-method push-token (lexer tok)
  (vector-push-extend tok tokens))

(define-lex-method read-number (L)
  (multiple-value-bind (num end-index) (parse-integer data :start index :junk-allowed t)
    (push-token L `(num ,num))
    (advance L (- end-index index))))

(define-lex-method read-variable (L)
  (loop for c = (peek l)
        with start = index
        while (or (alpha? c) (digit? c) (white-space? c))
        do (advance l)
        finally (let ((ident (trim-ident (subseq data start (1+ index)))))
                  (push-token l `(ident ,ident)))))

(define-lex-method read-keyword (L)
  (let* ((next (position *strop-char* data :start (1+ index)))
         (keyword-candidate (subseq data (1+ index) next))
         (iters (- next index)))
    (cond
      ((valid-stropped-keyword? keyword-candidate)
       (push-token L `(keyword ,(intern (string-upcase keyword-candidate))))
       (advance L iters))
      (t (error "Invalid keyword starting at ~a:~a" line-num column)))))

(define-lex-method advance (lexer &optional (n 1))
  (dotimes (i n)
    (when (new-line? (current-char lexer))
      (incf line-num)
      (setf column 0))
    (cond
      ((> (chars-left lexer) 0)
       (incf index))
      (t
       (setf finished t)))))

(define-lex-method chars-left (lexer &key (lookahead 0))
  (- (length data) (+ index lookahead 1)))

(define-lex-method current-char (lexer)
  (char data index))

;;;; The following functions are just character predicates
;;;; for the lexer along with some utility functions
;;; [a-zA-Z]
(defun alpha? (char)
  (alpha-char-p char))

;;; [0-9]
(defun digit? (char)
  (digit-char-p char 10))

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

(defun strop? (c)
  (char= c *strop-char*))

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
