(defpackage :utils
  (:use :cl :cl-user)
  (:export :remove-after))
(in-package :utils)

(defun remove-after (item list &key (test #'eql))
  (subseq list (search (list item) list :test test)))
