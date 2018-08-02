(defpackage :utils
  (:use :cl :cl-user)
  (:export :remove-after :nth-digit))
(defpackage :html-parser
  (:use :cl :cl-ppcre :trivia :utils)
  (:import-from :cl-ppcre :scan-to-strings)
  (:import-from :utils :dbind :mvbind))
(defpackage :bash
  (:use :cl :uiop)
  (:export :run-line
	   :*bash-output*))
(defpackage :workaround
  (:use :cl :bash)
  (:import-from :bash
		:run-line)
  (:export :http-request))
(defpackage :rss
  (:use :cl
	#-cl+ssl-broken :drakma
	#+cl+ssl-broken :workaround
	:utils
	:feedparser
	:html-parser)
  #-cl+ssl-broken (:import-from cl+ssl-broken :drakma :http-request)
  #+cl+ssl-broken (:import-from :workaround :http-request)
  (:import-from :utils :remove-after)
  (:export :update!
	   :update
	   :parse-feed))
(defpackage :youtube
  (:use :cl
	#+cl+ssl-broken :workaround
	#-cl+ssl-broken :drakma
	:utils
	:html-parser)
  (:import-from #+cl+ssl-broken :workaround
		#-cl+ssl-broken :drakma
		:http-request))

(defpackage :librivox
  (:use :cl
	;; Workaround for cl+ssl not working
	#+cl+ssl-broken :workaround
	#-cl+ssl-broken :drakma
	:html-parser
	:utils :uiop :youtube :rss :bordeaux-threads)
  #+cl+ssl-broken (:import-from :workaround :http-request)
  #-cl+ssl-broken (:import-from :drakma :http-request)
  (:import-from :rss :parse-feed
		:update
		:update!)
  (:import-from :utils :nth-digit)
  (:import-from :bash :run-line :*bash-output*)
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
