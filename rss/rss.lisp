(in-package :rss)
(defvar *persistence* "persistent.lisp")
(defun previous-update ()
  "Get the most recent update."
  (with-open-file (s *persistence*
		     :direction :input
		     :if-does-not-exist nil)
    (unless (null s) (read s nil nil))))
(defun update! (url)
  (previous-update))
(defun parse-feed (url)
  (feedparser:parse-feed (http-request url)))
(defun update (url)
  (remove-after (previous-update)
		(readable-feed (parse-feed url))
		:test #'equal))
