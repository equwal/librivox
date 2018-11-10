(in-package :bash)
(defvar *bash-output* *debug-io*)
(defun run-line (code &key (output t) (ignore-error-status t))
  (run-program code :output output :ignore-error-status ignore-error-status))
