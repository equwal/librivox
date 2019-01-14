(push :cl+ssl-broken cl::*features*)
;;(setf *features* (cons :multithreading cl::*features*))
(asdf:defsystem :librivox
  :serial t
  :depends-on (#-cl+ssl-broken
               :drakma
	       :cl-ppcre
               :uiop
               :query
               :bordeaux-threads
               :csv
               :xmls)
  :description "Librivox auto downloader/youtube uploader."
  :author       "Spenser Truex spensertruex.com"
  :components ((:file "packages")
               (:file "src/early")
               (:file "src/utils")
               (:file "src/bash")
	       (:file "src/workaround")
               (:file "src/api")
	       (:file "src/ffmpeg")
               (:file "src/uploader")
	       (:file "src/librivox")))
