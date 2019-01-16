(in-package :api)
;;; Low level constructor functions for managing the librivox API.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro nthfields (var)
    `(progn ,@(loop for x from 0 to (1- (funcall #'length var))
                    collect `(nthfield ',(nth x var)
                                       (lambda (list)
                                         (nth ,x list))))))
  (defun make-assoc (keys pairs)
    (loop for k in keys
          for p in pairs
          collect (cons k p)))
  (defun types (washers)
    "Generate a type list from the function names of the washers."
    (loop for w in washers
          collect (intern (symbol-name (car w)) (symbol-package :bogus))))
  (defmacro setfields (var fields)
    `(progn (defvar ,var ',fields)
            (setf ,var ',fields)
            (nthfields ,fields)))
  (setfields *fields* (description first_name id language
                                   last_name title url_zip_file)))
(defvar *temp* #p"/tmp/librivox-temp")
(defvar *last-update* #p"last-update.txt")
(defvar +epoch-offset+ 2208988800 "Seconds between Jan 1st 1900 (the universal epoch) and Jan 1st 1970 (the Unix epoch)")
(defvar *base-url* "https://librivox.org/api/feed/audiobooks/")
(defvar *exclude* '(#\< #\> #\& #\;)
  "Don't use these characters as escapes.")
(defun get-epoch-time ()
  "Get Unix epoch time: seconds from 01-01-1970."
  (- (get-universal-time) +epoch-offset+))

(defmacro washer (&rest washers)
  `(defwashers ,@(loop for w in washers
                       collect (list w))))
(defun apply-map (map list)
  "Sort a list according to an indexed map."
  (labels ((apply-map (map list acc)
             (if (null map)
                 (nreverse acc)
                 (apply-map (cdr map) list (push (nth (car map) list) acc)))))
    (apply-map map list nil)))
(defun gen-example-network (before after)
  "Make a sorting network from two example lists."
  (labels ((gen-example-map (before after acc)
             (if (null before)
                 (nreverse acc)
                 (gen-example-map (cdr before) after (push (position (car before) after) acc)))))
    (gen-example-map before after nil)))
(defun gen-map (pred list)
  "Make a mapping according to the sorting of the collection by the predicate."
  (gen-example-network list (sort (copy-list list) pred)))
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
                                       (list (symb 'wash- ty) str)))
                 (t ,str)))))))
(defmacro defwashers (&rest washers)
  (let ((gensyms (loop for x from 1 to (length washers)
                       collect (symb 'wash x))))
    `(progn ,@(loop for g in gensyms
                    for w in washers
                    collect `(defwasher ,g ,(types w) ,w))
            (apply #'make-wash ',(reverse gensyms)))))
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
(defun match (regex string)
  (multiple-value-bind (a b)
      (cl-ppcre:scan regex string)
    (when (and a b)
      (subseq string a b))))
(defun rmapcar (fn &rest args)
  "Map the leaves recursively."
  (if (some #'atom args)
      (apply fn args)
      (apply #'mapcar
             #'(lambda (&rest args)
                 (apply #'rmapcar fn args))
             args)))
(defun mapatoms (function tree)
  "Perform operations on the atoms of a tree."
  (labels ((y (tree)
             (cond ((null tree) nil)
 	           ((atom tree) (funcall function tree))
 	           (t (cons (y (car tree))
 		            (y (cdr tree)))))))
    (y tree)))
(defun flat (lst)
  (every #'atom lst))
