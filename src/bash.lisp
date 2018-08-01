(in-package :bash)
(defvar *bash-output* *debug-io*)
(defun run-line (code-string &key (output t))
  (run-program code-string :output output))
