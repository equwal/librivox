(defpackage :utils
  (:use :cl :uiop)
  (:export
   :symb
   :pack
   :only-one
   :mkstr
   :mvbind
   :dbind
   :dolines
   :with-gensyms
   :once-only
   :aif
   :awhen
   :select
   :nthline
   :length-lines
   :change-dir
   :run-line
   :mapfns))
(defpackage :workaround
  (:use :cl :utils)
  (:import-from :utils :run-line)
  (:export :http-request))
(defpackage :csv-specific
  (:use :cl :utils :csv :workaround)
  (:import-from :utils :only-one :nthline :mapfns :symb)
  (:import-from :workaround :http-request)
  (:export
   :nthidentifier
   :nthdescription
   :nthtitle
   :update
   :expand))
(defpackage :librivox
  (:use :cl
	;; Workaround for cl+ssl not working
	#+cl+ssl-broken :workaround
	#-cl+ssl-broken :drakma
	:utils :uiop :csv-specific)
  #+cl+ssl-broken (:import-from :workaround :http-request)
  #-cl+ssl-broken (:import-from :drakma :http-request)
  (:import-from
   :csv-specific
   :nthidentifier
   :nthdescription
   :nthtitle
   :update
   :expand)
  (:import-from
   :utils
   :only-one
   :mvbind :dbind :dolines
   :with-gensyms
   :once-only
   :aif
   :awhen
   :select
   :nthline
   :length-lines
   :change-dir
   :run-line)
  (:import-from :uiop :subdirectories :directory-files))
