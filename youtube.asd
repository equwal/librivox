(asdf:defsystem :youtube
  :serial t
  :description "Youtube data api."
  :depends-on (:drakma)
  :components ((:file "utils")
	       (:file "youtube/youtube")))
