(in-package #:algol60)

(defvar *file-name* #p"./example-programs/e_approx.algol")

(defvar *tokens* (make-array 256 :fill-pointer 0 :adjustable t))
(defvar *line-no* 0)
(defvar *col-no* 0)
(defvar *file-index* 0)
(defvar *file-data* nil)

;; Use underscore as the strop char
(defparameter *strop-character* #\_)

(defun make-token-array (&key (size 256))
  (make-array size :fill-pointer 0 :adjustable t))

(defun trim-ident (ident)
  (string-trim '(#\NewLine
                 #\Space
                 #\Tab) ident))

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

(defun colon? (char)
  (char= char #\:))

(defun semi-colon? (char)
  (char= char #\;))

(defun strop? (c)
  (char= c *strop-character*))

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

(defun lex (file-name)
  (reset-lexer-state)
  (read-file file-name)
  (loop for c = (current-char)
        while (chars-left?) do
           (cond
             ((strop? c) (read-keyword))
             ((alpha? c) (read-variable))
             ((comma? c) (push-token-adv 'comma))
             ((semi-colon? c) (push-token-adv 'semi-colon))
             ((dot? c) (push-token-adv 'dot))
             ((colon? c) (read-assign-op))
             ((white-space? c) (next-char))      ;Do nothing
             (t (next-char)))
        finally (return *tokens*)))

;; move the cursor forward after pushing the token
(defun push-token-adv (tok)
  (push-token tok)
  (next-char))

(defun read-assign-op ()
  (let ((next (peek-next)))
    (cond
      ((and
         (not (null next))
         (char= next #\=))
        (next-char)
        (next-char)
        (push-token '(op assign)))
      (t (error "Expected = here ~a:~a ~%" *line-no* *col-no*)))))

(defun peek-next ()
  (when (chars-left? :lookahead 1)
    (char *file-data* (1+ *file-index*))))

(defun push-token (tok)
  (vector-push-extend tok *tokens*))

(defun read-variable ()
  (flet ((read-while-valid-name ()
           (loop for c = (current-char) then (next-char)
                 with start = *file-index*
                 while (or (alpha? c)
                           (digit? c)
                           (white-space? c))
                 finally
                    (return (list
                             start *file-index*)))))
    (destructuring-bind (start end) (read-while-valid-name)
      (let ((ident (trim-ident (subseq *file-data* start end))))
        (push-token `(ident ,ident))))))

(defun read-keyword ()
  (destructuring-bind
      (start end init-col init-line) (read-til-next-strop)
    (let ((keyword-candidate (subseq *file-data* start end)))
      (cond
        ((valid-stropped-keyword? keyword-candidate)
         (push-token `(keyword ,(intern (string-upcase keyword-candidate)))))
        (t (error "Invalid keyword starting at ~a:~a" init-line init-col))))))


(defun read-til-next-strop ()
  (loop for cur-char = (next-char)
        with start-index = (1+ *file-index*)
        with init-line = *line-no*
        with init-col = *col-no*
        while (not (strop? cur-char))

        finally
           (next-char) ;;Push past ending stro
           (return (list start-index (1- *file-index*) init-col init-line))))

(defun next-char ()
  (when (chars-left?)
    (incf *file-index*)
    (incf *col-no*)
    (when (new-line? (current-char))
      (incf *line-no*)
      (setf *col-no* 0))
    (current-char)))

(defun chars-left? (&key (lookahead 0))
  (< (+ *file-index* lookahead) (1- (length *file-data*))))

(defun current-char ()
  (char *file-data* *file-index*))

(defun read-file (file-name)
  (setf *file-data* (read-file-into-string file-name)))

(defun reset-lexer-state ()
  (setf *tokens* (make-token-array))
  (setf *line-no* 0)
  (setf *col-no* 0)
  (setf *file-index* 0)
  (setf *file-data* nil))


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
