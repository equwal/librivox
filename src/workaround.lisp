(in-package :workaround)
;; Really would rather use a lisp library, but since BASH is so prevalent:
(defun http-request (url)
  (run-line (format nil "curl '~A'" url) :output 'string))
