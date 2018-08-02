(in-package :rss)
(defvar *persistence* "persistent.lisp")
(defun previous-update ()
  "Get the most recent update."
  (with-open-file (s *persistence*
		     :direction :input
		     :if-does-not-exist nil)
    (unless (null s)
      (car (read s nil nil)))))
(defun update! (update)
  (with-open-file (s *persistence*
		     :direction :output
		     :if-does-not-exist :create
		     :if-exists :overwrite)
      (prin1 update s)))
(defun parse-feed (url)
  (http-request url))
(defun update (url)
  (let ((prev (previous-update))
	(feed (readable-feed (parse-fed url))))
    (cond (prev (let ((removed (remove-after prev
					     :test #'equal)))
		  (if removed (butlast removed) feed)))
	  (t feed))))
