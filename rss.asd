(asdf:defsystem :rss
  :serial t
  :description "RSS feed reader."
  :depends-on (:drakma :cl-feedparser)
  :components ((:file "utils")
	       (:file "rss/rss")))
