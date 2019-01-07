(defpackage :csv
  (:use :cl :utils))
(defpackage :bash
  (:use :cl :uiop)
  (:export :run-line
	   :run-line-integer-output
	   :*bash-output*))
(defpackage :workaround
  (:use :cl :bash)
  (:import-from :bash
		:run-line)
  (:export :http-request))
(defpackage :librivox
  (:use :cl
	;; Workaround for cl+ssl not working
	#+cl+ssl-broken :workaround
	#-cl+ssl-broken :drakma
	:utils :uiop :bordeaux-threads)
  #+cl+ssl-broken (:import-from :workaround :http-request)
  #-cl+ssl-broken (:import-from :drakma :http-request)
  (:import-from :utils :mvbind :dbind :dolines
		:with-gensyms :once-only :aif :awhen :select)
  (:import-from :bash :run-line :*bash-output* :run-line-integer-output)
  (:import-from :uiop :subdirectories :directory-files)
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
