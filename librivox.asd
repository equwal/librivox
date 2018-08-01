(setf cl::*features* (cons :cl+ssl-broken cl::*features*))
;;(setf *features* (cons :multithreading cl::*features*))
(asdf:defsystem :librivox
  :serial t
  :depends-on (#-cl+ssl-broken :drakma
			       :cl-feedparser
			       :uiop
			       :bordeaux-threads)
  :description "Librivox auto downloader/youtube uploader."
  :components ((:file "packages")
	       (:file "utils")
	       (:file "src/bash")
	       (:file "src/workaround")
	       (:file "src/ffmpeg")
	       (:file "src/librivox")))

