(setf cl::*features* (cons :cl+ssl-broken cl::*features*))
;;(setf *features* (cons :multithreading cl::*features*))
(asdf:defsystem :librivox
  :serial t
  :depends-on (#-cl+ssl-broken :drakma
			       :cl-ppcre
			       :uiop
			       :bordeaux-threads
			       :utils)
  :description "Librivox auto downloader/youtube uploader."
  :components ((:file "packages")
	       (:file "src/csv")
	       (:file "src/bash")
	       (:file "src/workaround")
	       (:file "src/ffmpeg")
	       (:file "src/librivox")))

