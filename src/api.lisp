(in-package :api)
(defvar *temp* #p"/tmp/librivox-temp")
(defvar *last-update* #p"last-update.txt")
(defvar +epoch-offset+ 2208988800 "Seconds between Jan 1st 1900 (the universal epoch) and Jan 1st 1970 (the Unix epoch)")
(defvar *base-url* "https://librivox.org/api/feed/audiobooks/")
(defvar *exclude* '(#\< #\> #\& #\;)
  "Don't use these characters as escapes.")
(defun query (format &rest selector-value-pairs)
  "Produce a query to download the librivox content."
  (concat 'string
          *base-url*
          (let ((s (car selector-value-pairs))
                (v (cadr selector-value-pairs)))
            (if (eql s :fields)
                (? s ({ v))
                (? s v)))
          (& :format format)
          (mapcar #'(lambda (p) (& (if (eql (car p) :fields)
                                       ({ (car p))
                                       (car p))
                                   (cadr p)))
                  (cdr (group selector-value-pairs 2)))))
(defmacro nthfields (var)
  `(progn ,@(loop for x from 0 to (1- (funcall #'length var))
                  collect `(nthfield ',(nth x var)
                                     (lambda (list)
                                       (nth ,x list))))))
(defun pull (since &optional (track-stream *standard-output*)
                     (dir *downloads-dir*))
  (wash :xml (wget (query :xml
                          :since since
                          :fields (mapcar (compose #'string-downcase
                                                   #'symbol-name)
                                          *fields*))
                   "--no-verbose -O -"
                   track-stream
                   dir)))
(defun expand (path dir)
  "Convert librivox audiobooks into a directory tree filled with files
to be used in the downloading and conversion portions."
  (let ((len (length-lines path)))
    (loop for x from 0 to (1- len)
          do (mkdir (merge-pathnames dir
                                     (nthid x path))))))
(defun update-file (path format)
  "Get a new CSV to keep up with recent audiobook publishing."
  (with-open-file (s path :direction :output
                          :if-exists :append
                          :if-does-not-exist :create)
    (format s "~&~A" (http-request (query :since (get-epoch-time) format)))))

(defun get-epoch-time ()
  "Get Unix epoch time: seconds from 01-01-1970."
  (- (get-universal-time) +epoch-offset+))

(defun previous-time (path)
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
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun types (washers)
    "Generate a type list from the function names of the washers."
    (loop for w in washers
          collect (intern (symbol-name (car w)) (symbol-package :bogus))))
  (defmacro setfields (var fields)
    `(progn (defvar ,var ',fields)
            (setf ,var ',fields)
            (nthfields ,fields)))
  (setfields *fields* (id title description language url_zip_file
                          totaltimeseconds authors)))
(defun splist (sep str)
  "Splice a string 'SPliceLIST' into a list across a separator string."
  (labels ((inner (str acc)
             (if (string= "" str)
                 (nreverse acc)
                 (let ((r (search sep str)))
                   (if r
                       (inner (subseq str (1+ r))
                              (push (subseq str 0 r) acc))
                       (nreverse (push str acc)))))))
    (inner str nil)))
(defun make-wash (&rest fns)
  (defun wash (type code)
    (funcall (apply #'partial-1 type fns) code)))

(defmacro defwasher (name types fns)
  "Localised functions for washing inputs."
  (with-gensyms (type str)
    `(setf (symbol-function ',name)
           (lambda (,type ,str)
             (labels ,(loop for f in fns
                            collect (cons (symb 'wash- (car f))
                                          (cdr f)))
               (case ,type
                 ,@(loop for ty in types
                         collect (list ty
                                       (list (symb 'wash- ty) str)))))))))
(defmacro defwashers (&rest washers)
  (let ((gensyms (loop for x from 1 to (length washers)
                       collect (symb 'wash x))))
    `(progn ,@(loop for g in gensyms
                    for w in washers
                    collect `(defwasher ,g ,(types w) ,w))
            (apply #'make-wash ',(reverse gensyms)))))
(defun match (regex string)
  (multiple-value-bind (a b)
      (cl-ppcre:scan regex string)
    (when (and a b)
      (subseq string a b))))
(defun force-escape (str-or-escape)
  (if (escape-p str-or-escape)
      str-or-escape
      (make-escape-table (if (stringp str-or-escape)
                             str-or-escape
                             ""))))
(defun force-str (str-or-escape)
  (if (escape-p str-or-escape)
      (escape-str str-or-escape)
      (if (stringp str-or-escape)
          str-or-escape
          "")))
(defun mapatoms (function tree)
  "Perform operations on the atoms of a tree."
  (labels ((y (tree)
             (cond ((null tree) nil)
 	           ((atom tree) (funcall function tree))
 	           (t (cons (y (car tree))
 		            (y (cdr tree)))))))
    (y tree)))
(defwashers ;; Applied first to last. Use washers like an onion/filter.
    ((csv (csv) (mapcar #'(lambda (s) (splist "," s))
                        (splist (format nil "~%") csv)))
     (xml (xml)
          (let ((match (match "&.*?;" (force-str xml))))
            (if match
                (wash1 :xml (escape match (force-escape xml)))
                xml))))
    ((csv (csv) csv)
     (xml (xml) (cons (escape-tbl (force-escape xml))
                      (xmls:parse (force-str xml)
                                  :compress-whitespace nil))))
  ((csv (csv) csv)
   (xml (xml) (let ((tbl (car xml))
                    (xml (cdr xml)))
                (mapatoms #'(lambda (str)
                              (when str
                                (escape-str (unescape (make-escape
                                                       :tbl tbl
                                                       :str str)))))
                          xml))))
  ((csv (csv))))
;;; (wash :xml "<xml>test &gt;</xml>")
(defmacro dohash ((var tbl) (&key acc loc) &body body)
  `(loop for ,var being the ,(if (eql loc :key)
                                 'hash-key
                                 'hash-value) in ,tbl
         ,(if (eql acc :collect) 'collect 'do) (progn ,@body)))
(defun substitute-seq (new old source &optional (type 'string))
  "Like substitute but new and old are sequences, not elements."
  (let ((r (search old source)))
    (if r
        (substitute-seq new old
                        (concatenate type
                                     (subseq source 0 r)
                                     new
                                     (subseq source (+ r (length old)))))
        source)))
(defstruct escape tbl str)
(define-symbol-macro escape-tbl (escape-tbl escape))
(define-symbol-macro escape-str (escape-str escape))
(defun escape (raw escape)
  (let* ((char (genchar escape-str))
         (new-escape (funcall escape-tbl char raw)))
    (make-escape :tbl (escape-tbl new-escape)
                 :str (substitute-seq (string char)
                                      raw
                                      escape-str))))
(defun unescape (escape)
  (let ((tbl (funcall escape-tbl))
        (new (copy-seq escape-str)))
    (dohash (k tbl) (:acc :do :loc :key)
      (setf new (substitute-seq (gethash k tbl) (string k) new)))
    (make-escape :tbl tbl :str new)))
(defun make-escape-table (string)
  "Use a closure as a table of escapes."
  (let ((tbl (make-hash-table)))
    (labels ((fn (&optional escape-char value)
               (if escape-char
                   (if value
                       (progn (setf #1=(gethash escape-char tbl) value)
                              (make-escape :tbl #'fn :str string))
                       (make-escape :tbl #'fn :str string))
                   tbl)))
      (make-escape :tbl #'fn :str string))))
(defun genchar (string &optional (exclude *exclude*) (word-size 16));UTF8
  "Find a character not in the string--good for escaping things."
  (labels ((rec (int)
             (let ((char (code-char int)))
               (if (>= int word-size)
                   (error "Couldn't find an escape: is this the entire character set?")
                   (if (or (find char string)
                           (member char exclude :test #'char=))
                       (rec (1+ int))
                       char)))))
    (rec 0)))
