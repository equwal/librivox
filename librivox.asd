(asdf:defsystem :librivox
  :serial t
  :depends-on (:drakma :rss :youtube)
  :description "Librivox auto downloader/youtube uploader."
  :components ((:file "utils")
	       (:file "src/librivox")))
