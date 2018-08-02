(in-package :bash)
(defvar *bash-output* *debug-io*)
(defun run-line (code-string &key (output t) ignore-error-status)
  (run-program code-string :output output :ignore-error-status t))
