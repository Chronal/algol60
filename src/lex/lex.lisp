(in-package algol60/lex)

;;;; Core class driving the lexing
;;;; Ideally later it can be configurable
;;;; to support different strop chars
(defclass lexer ()
  ((line-num :initform 1
             :accessor lex-line)
   (col-num :initform 0
            :accessor lex-col)
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
   (tokens
    :initform (make-array +token-buf-init-len+ :fill-pointer 0 :adjustable t)
    :type (vector token))))

(defun make-lexer (source-code)
  (make-instance 'lexer :src source-code))

(defmethod initialize-instance :after ((lexer lexer) &key)
  (with-slots (data src-len) lexer
    (setf src-len (length data))))

(defmethod print-object ((L lexer) stream)
  (print-unreadable-object (L stream :type t :identity t)
    (with-slots (line-num col-num index) L
      (format stream "Lexer at ~a:~a, index ~a" line-num col-num index))))

(defmethod lex-reset ((lex lexer))
  (with-slots (line-num
               col-num
               start-tok index
               tokens) lex
    (setf line-num 1)
    (setf col-num 0)
    (setf start-tok 0)
    (setf index 0)
    (setf (fill-pointer tokens) 0)))

(defmethod at-end? ((lex lexer) &key (offset 0))
  (with-slots (index src-len) lex
    (>= (+ offset index) src-len)))

(defmethod advance ((lex lexer))
  (let ((c (peek lex)))
    (incf (lex-index lex))
    (case c
      (#\Newline (progn
                   (incf (lex-line lex))
                   (setf (lex-col lex) 0)))
      (otherwise (incf (lex-col lex))))
    c))

(defmethod match ((lex lexer) c)
  (cond
    ((at-end? lex) nil)
    ((char/= c (peek lex)) nil)
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

(defmethod last-token ((lex lexer))
  (with-slots (tokens) lex
    (aref tokens (1- (length tokens)))))

(defmethod pop-token ((lex lexer))
  (with-slots (tokens) lex
    (vector-pop tokens)))

(defmethod add-token ((lex lexer) tok)
  (let ((token (if (symbolp tok)
                   (simple-token tok)
                   tok)))
    (vector-push-extend token (slot-value lex 'tokens))
    token))

(defmethod add-string-token ((lex lexer) str)
  (if (eql 'string (token-type (last-token lex)))
      (let ((token (pop-token lex)))
        (add-token lex
                   (make-token :type 'string
                               :data (concatenate 'string (token-data token) str))))
      (add-token lex
                 (make-token :type 'string
                             :data str))))

(defmethod add-ident-token ((lex lexer) ident)
  (add-token lex (make-token
                  :type 'ident
                  :data (string-trim '(#\Newline #\Space #\Tab) ident))))

(defmethod scan-token ((lex lexer))
  (setf (lex-tok-start lex) (lex-index lex))
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

      ;; Brackets
      (#\( (add-token lex 'open-paren))
      (#\) (add-token lex 'close-paren))
      (#\[ (add-token lex 'open-subscript))
      (#\] (add-token lex 'close-subscript))
      (#\` (scan-string lex))
      (t
       (cond
         ((alpha? c) (scan-ident lex))
         ((digit? c) (scan-number lex)))))))

(defmethod scan-string ((lex lexer))
  (iter
    (with in-string = 1)
    (for next-char = (peek lex))
    (while (>= in-string 1))
    (cond 
      ((char= next-char #\`) (incf in-string))
      ((char= next-char #\') (decf in-string)))
    (advance lex))

  (add-string-token lex  (subseq (lex-src lex)
                                 (1+ (lex-tok-start lex))
                                 (1- (lex-index lex)))))

(defmethod scan-ident ((lex lexer))
  (let ((ident
          (make-array 16 :element-type 'character :adjustable t :fill-pointer 0))
        (src (lex-src lex)))

    (vector-push (aref src (1- (lex-index lex))) ident)
    
    (iter
      (with ident-start = (lex-tok-start lex))
      (with last-whitespace = nil)

      (for c = (peek lex))
      (for index = (lex-index lex))

      (until (at-end? lex))

      (cond
        ((alnum? c) (vector-push-extend c ident))
        ((whitespace? c)
         (when-let (keyword
                    (keyword? (subseq src (or last-whitespace ident-start) index)))

           (unless (null last-whitespace)
             (decf (fill-pointer ident) (- index last-whitespace))
             (add-ident-token lex ident))
           
           (add-token lex keyword)
           (leave))

         (setf last-whitespace (1+ index)))
        (t (finish)))


      (after-each
       (advance lex))

      (finally
       (if-let ((keyword (keyword? ident)))
         (add-token lex keyword)
         (if-let (keyword 
                  (keyword? (subseq
                             src
                             (or last-whitespace ident-start)
                             index)))
           (progn
             (add-ident-token lex
                              (subseq src ident-start last-whitespace))
             (add-token lex keyword))

           (add-ident-token lex ident)))))))
;;; 
;;; TODO This just does till \n for now
(defmethod scan-end-comment ((lex lexer))
  (iter
    (while (char/= (peek lex) #\Newline))
    (advance lex))
  (advance lex)) ; Consume new line

(defmethod scan-comment ((lex lexer))
  (iter
    (while (and (not (at-end? lex))
                (char/= (peek lex) #\;)))
    (advance lex))
  ;; Consume the ;
  (advance lex))

;;; TODO extend to handle spaces inside integer
;;; like components
(defmethod scan-number ((lex lexer))
  (iter
    (while (digit? (peek lex)))
    (advance lex))
  
  (if (match lex #\.)
      (progn
        (advance lex)
        (iter
          (while (digit? (peek lex)))
          (advance lex))
        (when (match lex #\e)
          (advance lex)
          (when (sign? (peek lex))
            (advance lex)))
        (iter
          (while (digit? (peek lex)))
          (advance lex))

        (with-accessors ((index lex-index)
                         (src lex-src)
                         (tok-start lex-tok-start)) lex
          (add-token lex
                     (make-token :type 'real
                                 :data (parse-float
                                        (subseq src tok-start index))))))

      (with-accessors ((index lex-index)
                       (src lex-src)
                       (tok-start lex-tok-start)) lex
        (add-token lex
                   (make-token
                    :type 'integer
                    :data (parse-integer
                           (subseq src tok-start index)))))))

(defmethod scan-tokens ((lex lexer))
  (lex-reset lex)
  (iter (while (not (at-end? lex)))
    (scan-token lex))
  (slot-value lex 'tokens))

(defun tokenise-string (src)
  (let ((lex (make-lexer src)))
    (scan-tokens lex)))

(defun tokenise-file (file-path)
  (when-let* ((fpath (probe-file file-path))
              (fdata (alex:read-file-into-string fpath)))
    (tokenise-string fdata)))
