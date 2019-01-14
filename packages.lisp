(defpackage :utils
  (:use :cl :uiop)
  (:export
   :concat
   :flatten
   :select
   :mkdir
   :length-lines
   :interpol
   :symb
   :dodir
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
   :length-lines
   :change-dir
   :run-line
   :defcollect
   :mapfns))
(defpackage :workaround
  (:use :cl :utils)
  (:import-from :utils :run-line)
  (:export :http-request))
(defpackage :csv-specific
  (:use :cl :utils :csv :workaround :query)
  (:import-from :utils :length-lines :only-one :mapfns :symb :defcollect)
  (:import-from :workaround :http-request)
  (:import-from :query :{ :& :?)
  (:import-from :csv :list->csv)
  (:export
   :nthid
   :nthtitle
   :nthdescription
   :nthlanguage
   :nthurl_zip_file
   :nthtotaltimeseconds
   :nthauthors
   :update
   :expand))
(defpackage :librivox
  (:use :cl
	;; Workaround for cl+ssl not working
	#+cl+ssl-broken :workaround
	#-cl+ssl-broken :drakma
	:utils :uiop :csv-specific :query)
  #+cl+ssl-broken (:import-from :workaround :http-request)
  #-cl+ssl-broken (:import-from :drakma :http-request)
  (:import-from :query :? :& :{ :csvs :search-keys :escape :escapes)
  (:import-from
   :csv-specific
   :nthid
   :nthtitle
   :nthdescription
   :nthlanguage
   :nthurl_zip_file
   :nthtotaltimeseconds
   :nthauthors
   :update
   :expand)
  (:import-from
   :utils
   :flatten
   :select
   :concat
   :mkdir
   :interpol
   :dodir
   :mvbind :dbind :dolines
   :with-gensyms
   :once-only
   :aif
   :awhen
   :select
   :length-lines
   :change-dir
   :run-line)
  (:import-from :uiop :subdirectories :directory-files))
