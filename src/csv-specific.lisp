(in-package :csv-specific)
(defvar *down* nil)
(defvar *temp* #p"/tmp/librivox-temp")

(defun nthcsv (n path &key from-end)
  (csv:convert-string (nthline n path :from-end from-end)))
(defmacro nthfield (name fn)
  "Writing field access definitions."
  `(defun ,(symb 'nth name)
       (n path &key from-end)
     (,fn (nthcsv n path :from-end from-end))))
(nthfield identifier second)
(nthfield description first)
(nthfield title third)

(defun update-file (path &key (forward t))
  "Get a new CSV to keep up with recent audiobook publishing."
  (with-open-file (s path :direction :output
                          :if-exists (if forward :append :overwrite)
                          :if-does-not-exist :create)
    (format s "~&~A" (http-request (query :forward forward)))))
(defun query (&key forward all)
  "Produce a query to download the librivox content."
  (cond ((and forward (not all)) )
        ((and (not forward) all) )
        (t
         (error "Unable to generate a CSV query. Please specify :all or :forward in args"))))
(defun expand (path dir)
  "Convert a CSV of librivox audiobooks into a directory tree filled with files
to be used in the downloading and conversion portions."
  (let ((len (length-lines path)))
    (loop for x from 0 to (1- len)
          do (mkdir (merge-pathnames *downloads-dir*
                                     (nthidentifier x path))))))
(defun update (path &key (forward t))
  (mapfns (path forward) #'update-file #'expand))
