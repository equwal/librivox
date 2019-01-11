(push :cl+ssl-broken cl::*features*)
;;(setf *features* (cons :multithreading cl::*features*))
(asdf:defsystem :librivox
  :serial t
  :depends-on (#-cl+ssl-broken
               :drakma
	       :cl-ppcre
               :uiop
               :bordeaux-threads
               :csv)
  :description "Librivox auto downloader/youtube uploader."
  :author       "Spenser Truex spensertruex.com"
  :components ((:file "packages")
               (:file "src/utils")
               (:file "src/bash")
	       (:file "src/workaround")
	       (:file "src/ffmpeg")
               (:file "src/uploader")
               (:file "src/csv-specific")
	       (:file "src/librivox")))

