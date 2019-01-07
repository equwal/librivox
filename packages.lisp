(defpackage :workaround
  (:use :cl)
  (:export :http-request))
(defpackage :librivox
  (:use :cl
	;; Workaround for cl+ssl not working
	#+cl+ssl-broken :workaround
	#-cl+ssl-broken :drakma
	:utils :uiop :bordeaux-threads)
  #+cl+ssl-broken (:import-from :workaround :http-request)
  #-cl+ssl-broken (:import-from :drakma :http-request)
  (:import-from
   :utils
   :mvbind :dbind :dolines
   :with-gensyms
   :once-only
   :aif
   :awhen
   :select
   :run-line)
  (:import-from :uiop :subdirectories :directory-files)
  (:import-from
   :bordeaux-threads
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
