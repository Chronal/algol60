(in-package #:algol60)

(defconstant +default-buffer-length+ 256)

(defmacro while (test &rest body)
  "Repeat body while test is true. Taken from paip."
  `(loop (unless ,test (return nil)
                 ,@body)))

(defun make-token-array (&key (size 256))
  (make-array size :fill-pointer 0 :adjustable t))
