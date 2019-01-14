(in-package :csv-specific)
;;; Librivox API info: https://librivox.org/api/info
(defvar *down* nil)
(defvar *temp* #p"/tmp/librivox-temp")
(defvar *last-update* #p"last-update.txt")
(defvar +epoch-offset+ 2208988800 "Seconds between Jan 1st 1900 (the universal epoch) and Jan 1st 1970 (the Unix epoch)")

(nthfields (id title description language url_zip_file totaltimeseconds authors))
(defmacro nthfields (var)
  `(progn ,@(loop for x from 0 to (1- (funcall #'length var))
                  collect `(nthfield ',(nth x var)
                                     (lambda (list) (nth ,x list))))))
;;; (progn (nthfield 'id (lambda (list) (nth 0 list))) ...)

(defun update-file (path)
  "Get a new CSV to keep up with recent audiobook publishing."
  (with-open-file (s path :direction :output
                          :if-exists :append
                          :if-does-not-exist :create)
    (format s "~&~A" (http-request (query :since )))))
(defun query (selector value &key (fields *fields*) (format :csv))
  "Produce a query to download the librivox content."
  (concatenate 'string
               "https://librivox.org/api/feed/audiobooks/"
               (? selector value)
               (if fields
                   (& :fields ({ (csvs (apply #'symbol-name fields))))
                   "")
               (& :format format)))

(defun expand (path dir)
  "Convert a CSV of librivox audiobooks into a directory tree filled with files
to be used in the downloading and conversion portions."
  (let ((len (length-lines path)))
    (loop for x from 0 to (1- len)
          do (mkdir (merge-pathnames *downloads-dir*
                                     (nthid x path))))))
(defun get-epoch-time ()
  "Get Unix epoch time: seconds from 01-01-1970."
  (- (get-universal-time) +epoch-offset+))

(defun previous-time (path time)
  (with-open-file (s path)
    (parse-integer (read-line s nil nil))))
(defun save-time (path time)
  (with-open-file (s path :direction :output
                          :if-exists :overwrite
                          :if-does-not-exist :create)
    (prin1 time)))

(defun update (path &key (forward t))
  (let ((before (get-epoch-time)))
    (mapfns (path forward) #'update-file #'expand)
    (save-time *last-update* before)))
