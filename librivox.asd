(asdf:defsystem :librivox
  :serial t
  :depends-on (:drakma :uiop :rss :youtube)
  :description "Librivox auto downloader/youtube uploader."
  :components ((:file "utils")
	       (:file "src/librivox")
	       (:file "src/ffmpeg")))

