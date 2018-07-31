(defpackage :rss
  (:use :cl :drakma :utils :cl-feedparser)
  (:import-from :drakma
		:http-request)
  (:import-from :utils
		:remove-after)
  ;(:import-from :feedparser)
  ;(:export )
  )
(in-package :rss)
(defvar *persistence* "persistent.lisp")
#|(let ((x (parse-feed "https://librivox.org/rss/latest_releases")))
   (loop for hk being the hash-key in x
      for hv being the hash-value in x
      collect (cons hk hv)))
(defun parse-feed (url)
  (feedparser:parse-feed (http-request url)))
(defun update (url)
  (remove-after (previous-update)
		(readable-feed (parse-feed url))
		:test #'equal))|#
(defun previous-update ()
  "Get the most recent update."
  (with-open-file (s *persistence*
		     :direction :input
		     :if-does-not-exist :create)
    (read s nil nil)))
