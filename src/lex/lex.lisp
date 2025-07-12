(in-package algol60/lex)

;;;; Core class driving the lexing
;;;; Ideally later it can be configurable
;;;; to support different strop chars
(defclass lexer ()
  ((line-num :initform 1)
   (start-tok
    :accessor lex-tok-start
    :initform 0)
   (index
    :accessor lex-index
    :initform 0)
   (data :initarg :src
         :accessor lex-src
         :initform (error "Source code to be tokenised must be provided"))
   (src-len :reader src-len)
   (finished :initform nil)
   (tokens
    :initform (make-array +token-buf-init-len+ :fill-pointer 0 :adjustable t))))

(defun make-lexer (source-code)
  (make-instance 'lexer :src source-code))

(defmethod initialize-instance :after ((lexer lexer) &key)
  (with-slots (data src-len) lexer
    (setf src-len (length data))))

(defmethod print-object ((L lexer) stream)
  (print-unreadable-object (L stream :type t :identity t)
    (with-slots (line-num index finished) L
      (format stream "Line #~a, index ~a, finished? ~a" line-num index finished))))

(defmethod lex-reset ((lex lexer))
  (with-slots (line-num start-tok index
               finished tokens) lex
    (setf line-num 1)
    (setf start-tok 0)
    (setf index 0)
    (setf finished nil)
    (setf (fill-pointer tokens) 0)))

(defmethod at-end? ((lex lexer) &key (offset 0))
  (with-slots (index src-len) lex
    (>= (+ offset index) src-len)))

(defmethod advance ((lex lexer))
  (let ((c (current lex)))
    (incf (lex-index lex))
    c))

(defmethod current ((lex lexer))
  (with-accessors ((src lex-src)
                   (index lex-index)) lex
    (aref src index)))

(defmethod match ((lex lexer) c)
  (cond
    ((at-end? lex) nil)
    ((char/= c (current lex)) nil)
    (t (incf (lex-index lex))
       t)))

(defmethod match-str ((lex lexer) str))

(defmethod peek-ahead ((lex lexer) &key (ahead 0))
  (with-accessors ((src lex-src)
                   (src-len src-len)
                   (index lex-index)) lex
    (let ((peek-index (+ index ahead)))
      (if (>= peek-index src-len)
          #\0
          (aref src peek-index)))))

(defmethod peek ((lex lexer))
  (peek-ahead lex))

(defmethod peek-next ((lex lexer))
  (peek-ahead lex :ahead 1))

(defmethod add-token ((lex lexer) tok)
  (vector-push-extend tok (slot-value lex 'tokens)))

(defmethod scan-token ((lex lexer))
  (let ((c (advance lex)))
    (case c
      ;; Arithmetic
      (#\+ (add-token lex 'add))
      (#\- (add-token lex 'sub))
      (#\* (add-token lex 'mult))
      (#\/ (add-token lex 'div))
      (#\^ (add-token lex 'pow))

      ;; Relational
      (#\< (add-token lex (if (match lex #\=) 'lte 'lt)))
      (#\> (add-token lex (if (match lex #\=) 'gte 'gt)))
      (#\= (add-token lex (cond
                            ((match lex #\>) 'implies)
                            ((match lex #\=) 'equivalent)
                            (t 'equal))))

      ;; Logical
      (#\& (add-token lex 'and))
      (#\| (add-token lex 'or))
      (#\! (add-token lex (if (match lex #\=) 'not-equal 'negate)))

      ;; Separators
      (#\, (add-token lex 'comma))
      (#\. (add-token lex 'dot))
      (#\: (add-token lex (if (match lex #\=) 'becomes 'colon)))
      (#\; (add-token lex 'semi-colon))

      ;; Whitespace
      ((#\Tab #\Space #\Return) t)
      (#\Newline (incf (slot-value lex 'line-num)))

      ;; Brackets
      (#\( (add-token lex 'open-paren))
      (#\) (add-token lex 'close-paren))
      (#\[ (add-token lex 'open-subscript))
      (#\] (add-token lex 'close-subscript))
      (#\` (scan-string lex))
      (otherwise
       (cond
         ((alpha? c) (scan-ident lex))
         ((digit? c) (scan-number lex)))))))

;;; TODO: Add support for nested string with ` for open string
;;; and ' for close string
(defmethod scan-string ((lex lexer))
  (loop while (char/= (peek lex) #\') do
    (advance lex))
  (advance lex)
  (with-accessors ((index lex-index)
                   (src lex-src)
                   (tok-start lex-tok-start)) lex

    (add-token lex `(string ,(subseq src (1+ tok-start) index)))))

(defmethod scan-ident ((lex lexer))
  (loop while (alnum? (peek lex)) do
    (advance lex))
  (with-accessors ((index lex-index)
                   (src lex-src)
                   (tok-start lex-tok-start)) lex
    (let ((ident (subseq src tok-start index)))
      (alex:if-let (keyword (keyword? ident))
        (case keyword
          (comment (scan-comment lex))
          (end (progn
                 (scan-end-comment lex)
                 'end))
          (otherwise (add-token lex keyword)))
        (add-token lex `(ident ,ident))))))

;;; TODO This just does till \n for now
(defmethod scan-end-comment ((lex lexer))
  (adv-while lex (lambda () (char/= (peek lex) #\Newline)))
  (advance lex) ; Consume new line
  (incf (slot-value lex 'line-num)))

(defmethod scan-comment ((lex lexer))
  (loop
    while (and (not (at-end? lex))
               (char/= (peek lex) #\;))
    do
       (when (char= (peek lex) #\Newline)
         (incf (slot-value lex 'line-num)))
       (advance lex))
  ;; Consume the ;
  (advance lex))

;;; TODO extend to handle spaces inside integer
;;; like components
(defmethod scan-number ((lex lexer))
  (adv-while lex (lambda () (digit? (peek lex))))
  (if (match lex #\.)
      (progn
        (advance lex)
        (adv-while lex (lambda () (digit? (peek lex))))

        (when (match lex #\e)
          (advance lex)
          (when (sign? (peek lex))
            (advance lex))
          (adv-while lex (lambda () (digit? (peek lex)))))

        (with-accessors ((index lex-index)
                         (src lex-src)
                         (tok-start lex-tok-start)) lex
          (add-token lex
                     `(real
                       ,(parse-float
                         (subseq index tok-start index))))))

      (with-accessors ((index lex-index)
                       (src lex-src)
                       (tok-start lex-tok-start)) lex
        (add-token lex
                   `(integer
                     ,(parse-integer
                       (subseq src tok-start index)))))))

(defmethod adv-while ((lex lexer) pred)
  (loop while (funcall pred) do
    (advance lex)))

(defmethod scan-tokens ((lex lexer))
  (lex-reset lex)
  (loop while (not (at-end? lex)) do
    (setf (lex-tok-start lex) (lex-index lex))
    (scan-token lex))
  (slot-value lex 'tokens))

(defun tokenise-string (src)
  (let ((lex (make-lexer src)))
    (scan-tokens lex)))

(defun tokenise-file (file-path)
  (alex:when-let* ((fpath (probe-file file-path))
                   (fdata (alex:read-file-into-string fpath)))
    (tokenise-string fdata)))
