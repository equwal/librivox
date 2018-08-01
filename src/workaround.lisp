(in-package :workaround)
(defun http-request (url)
  (run-line (format nil "curl '~A'" url) :output 'string))
