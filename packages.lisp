(defpackage :utils
  (:use :cl :cl-user)
  (:export :remove-after :nth-digit))
(defpackage :rss
  (:use :cl :drakma :utils :cl-feedparser)
  (:import-from :drakma :http-request)
  (:import-from :utils :remove-after)
  ;;(:import-from :feedparser)
  ;;(:export )
  )
(defpackage :youtube
  (:use :cl :drakma :utils))

(defpackage :bash
  (:use :cl :uiop :utils)
  (:import-from :utils :nth-digit)
  (:export :convert
	   :processor-cores))

(defpackage :librivox
  (:use :cl :drakma :utils :uiop :youtube :rss :bordeaux-threads)
  (:import-from :uiop :run-program)
  (:import-from :bash :convert :processor-cores)
  (:import-from :bordeaux-threads
		:make-thread
		:make-lock
		:join-thread
		:thread-yield
		:make-condition-variable
		:condition-wait
		:condition-notify
		:all-threads
		:thread-alive-p
		:destroy-thread
		:interrupt-thread
		:join-thread))
