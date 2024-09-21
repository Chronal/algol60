(in-package #:algol60)

(defmacro while (test &rest body)
  "Repeat body while test is true. Taken from paip."
  `(loop (unless ,test (return nil)
                 ,@body)))
