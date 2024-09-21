(in-package #:algol60)

(defvar *file-name* #p"./example-programs/e_approx.algol")
(defvar *lex* (make-lexer *file-name*))

;; [A-Za-z]
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

(defun semi-colon? (char)
  (char= char #\;))

(defun strop-char? (char)
  (char= char *strop-character*))

(defun white-space? (char)
  (or (char= char #\Newline)
      (char= char #\Space)
      (char= char #\Tab)))

(defun new-line? (char)
  (char= char #\Newline))

(defun valid-stropped-keyword? (str)
  (labels
      ((equal-to-input? (keyword)
         (string= keyword str)))
    (some #'equal-to-input?
          '(
            ;; Booleans
            "true"
            "false"
            ;; Sequential operators
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


(defclass lexer ()
  ((file-name :initarg :file-name
              :initform (error "Must supply file name"))
   (file-data :accessor lex-buffer)
   ;; Positional state
   (line-num :initform 0 :accessor lex-line)
   (column-num :initform 0 :accessor lex-col)
   (file-index :initform 0 :accessor lex-index)

   (cur-char :accessor lex-char)
   (tokens :accessor lex-tokens)))


(defun make-lexer (file-name)
  (make-instance 'lexer :file-name file-name))

(defmethod initialize-instance :after ((lex lexer) &key)
  (with-accessors ((tokens lex-tokens)
                   (cur-char lex-char)
                   (findex lex-index)
                   (fdata lex-buffer)) lex
    (setf fdata
          (read-file-into-string (slot-value lex 'file-name)))
    (setf tokens (make-array 100 :fill-pointer 0 :adjustable t))
    (setf cur-char (char fdata findex))))

;; Should return a list of tokens
(defmethod run ((lex lexer)))
(defmethod next-char ((lex lexer)))
(defmethod keyword ((lex lexer)))
(defmethod scan-til-char ((lex lexer) char))

(defun lex (file-name)
  (let*
      ((file-data (alexandria:read-file-into-string file-name))
       (file-index 0)
       (line-no 0)
       (tokens (make-array 100 :fill-pointer 0 :adjustable t))
       (cur-char (char file-data file-index)))

    (labels
        ((next-char ()
           (incf file-index)
           (setf cur-char (char file-data file-index))
           (when (new-line? cur-char)
             (incf line-no)))
         (read-till (pred)
           (do ()
               (pred)
            next-char)))

      (loop
        (setf cur-char (char file-data file-index))
        (cond
          ((strop-char? cur-char) (read-till (lambda (x) (strop-char? x)))))))))


;;; Types of Tokens
;;; '(ident ...)
;;; '(keyword ...)
;;;
;;;
;;; Punctuation
;;; , => 'comma
;;; ";" => 'semi-colon
;;; . => 'dot
;;; Operators
;;; + => 'add
;;; * => 'mult
;;; - => 'sub
;;; / => 'div
;;; & => 'and
;;; | => 'or
;;; > => 'more-than
;;; >= => more-than-eq
;;; < => 'less-than
;;; <= => 'less-than-eq
;;; = => 'eq
;;; ~= => 'not-eq
;;;
;;; Brackets
;;; 'open_paren
;;; 'close_paren
;;; 'open_string
;;; 'close_string
;;; 'open_block => _begin_
;;; 'close_block => _end_
