(asdf:defsystem :librivox
  :serial t
  :depends-on (:drakma)
  :pathname "src/"
  :components ((:file "utils")
	       (:file "librivox")))
(asdf:defsystem :youtube
  :serial t
  :depends-on (:drakma)
  :pathname "src/"
  :components ((:file "utils")
	       (:file "youtube")))
(asdf:defsystem :uploader
  :serial t
  :depends-on (:drakma :librivox)
  :pathname "src/"
  :components ((:file "uploader")))
