(defpackage :librivox
  (:use :cl :drakma :utils :uiop)
  (:import-from :uiop
		:run-program))
(in-package :librivox)
(defvar *url* "https://librivox.org/rss/latest_releases")
