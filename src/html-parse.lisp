(in-package :html-parser)
;;; Specification:
;;; (("xml")("html" ("body" ("p" nil)))
;;; (ie. each one is a regex.
(defun tag-of (html-string)
  (mvbind (success? tag-array)
	  (scan-to-strings"<(.*?\\w*).*?>" html-string)
	  (when success? (aref tag-array 0))))
(defun content-of (tag-name html-string &key inside after)
  (flet ((inner () (mvbind (output parens)
			   ;; If things are slow, it is this runtime regex expansion
			   (scan-to-strings (format nil "<~A\\w*.*?>(.*?)</~:*~A>(.*)" tag-name) html-string)
			   (declare (ignore output))
			   (let ((inside-val (aref parens 0))
				 (after-val (aref parens 1)))
			     (values inside-val after-val)))))
    (mvbind (inside1 after1) (inner)
	    (if (and inside after)
		(values inside1 after1)
		(if inside inside1 after1)))))
(defun make-node (left right)
  (cons left right))
(defun make-tree (node1 node2)
  (cons node1 node2))

(defun make-datum (id text)
  (make-node id text))
(defun html->lispy (html-string)
  (if (string= "" html-string)
      nil
      (let ((tag-of (tag-of html-string)))
	(if tag-of
	    (make-tree (make-datum tag-of (html->lispy (content-of tag-of
								   html-string
								   :inside t)))
		       (html->lispy (content-of tag-of html-string :after t)))
	    html-string))))
