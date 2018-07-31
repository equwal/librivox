(asdf:defsystem :librivox
  :serial t
  :depends-on (:drakma :uiop :cl-feedparser :bordeaux-threads)
  :description "Librivox auto downloader/youtube uploader."
  :components ((:file "packages")
	       (:file "utils")
	       (:file "src/bash")
	       (:file "src/librivox")))

