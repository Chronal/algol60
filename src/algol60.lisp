(in-package algol60)

(defun run (file)
  (let* ((src (alexandria:read-file-into-string file))
         (tokens (tokenise-string src))
         (ast (parse tokens)))
    (print ast)))
