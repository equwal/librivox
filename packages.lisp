(defpackage :early
  (:use :cl)
  (:export :*downloads-dir*))
(defpackage :utils
  (:use :cl :uiop :early)
  (:import-from :early :*downloads-dir*)
  (:export
   :partial-1
   :compose
   :run-line*
   :run-line*-integer-output
   :wget
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
   :group
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
  (:use :cl :utils :early)
  (:import-from :early :*downloads-dir*)
  (:import-from :utils :run-line)
  (:export :http-request))
(defpackage :api
  (:use :cl :utils :csv :cl-ppcre :workaround :query :early)
  (:import-from :cl-ppcre :regex-replace :scan :regex-replace-all)
  (:import-from :utils :partial-1 :compose :group :length-lines :only-one :mapfns :symb :defcollect)
  (:import-from :early :*downloads-dir*)
  (:import-from :workaround :http-request)
  (:import-from :query :{ :& :?)
  (:import-from :csv :list->csv)
  (:export
   :escape
   :librivox-pull
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
	:utils :uiop :query :xmls :api)
  #+cl+ssl-broken (:import-from :workaround :http-request)
  #-cl+ssl-broken (:import-from :drakma :http-request)
  (:import-from :xmls :parse)
  (:import-from :query :? :& :{ :csvs :search-keys :escape :escapes)
  (:import-from
   :api
   :nthid
   :nthtitle
   :nthdescription
   :nthlanguage
   :nthurl_zip_file
   :nthtotaltimeseconds
   :nthauthors
   :update
   :expand
   :librivox-pull)
  (:import-from :early :*downloads-dir*)
  (:import-from
   :utils
   :run-line*
   :run-line*-integer-output
   :wget
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
