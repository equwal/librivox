(in-package :api)
(defvar *temp* #p"/tmp/librivox-temp")
(defvar *last-update* #p"last-update.txt")
(defvar +epoch-offset+ 2208988800 "Seconds between Jan 1st 1900 (the universal epoch) and Jan 1st 1970 (the Unix epoch)")
(defvar *base-url* "https://librivox.org/api/feed/audiobooks/")
(defmacro nthfields (var)
  `(progn ,@(loop for x from 0 to (1- (funcall #'length var))
                  collect `(nthfield ',(nth x var)
                                     (lambda (list) (nth ,x list))))))
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
(macrolet ((letfields (fields &body body)
             `(let ((fields ',fields))
                (nthfields ,fields)
                ,@body)))
  ;; This is anaphoric setup is to only define the fields in one place
  (letfields
   (id title description language url_zip_file totaltimeseconds authors)
   (defun librivox-pull (since &optional (track-stream *standard-output*)
                                 (dir *downloads-dir*))
     (wash :xml (wget (query :xml
                             :since since
                             :fields (mapcar (compose #'string-downcase
                                                      #'symbol-name)
                                             fields))
                      "--no-verbose -O -"
                      track-stream
                      dir)))))
(defun update-file (path format)
  "Get a new CSV to keep up with recent audiobook publishing."
  (with-open-file (s path :direction :output
                          :if-exists :append
                          :if-does-not-exist :create)
    (format s "~&~A" (http-request (query :since (get-epoch-time) format)))))

(defun expand (path dir)
  "Convert librivox audiobooks into a directory tree filled with files
to be used in the downloading and conversion portions."
  (let ((len (length-lines path)))
    (loop for x from 0 to (1- len)
          do (mkdir (merge-pathnames dir
                                     (nthid x path))))))
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
(defwasher wash1 (:csv :xml)
  ((csv (csv) csv)
   (xml (xml)
        (cl-ppcre:regex-replace "&.*?;" xml " "))))
(defwasher wash2 (:csv :xml)
  ((csv (csv) (mapcar #'(lambda (s) (splist "," s))
                      (splist (format nil "~%") csv)))
   (xml (xml) )))
(defun wash (type code)
  "Just cleanup the code to be uniform."
  (wash2 type (wash1 type code)))
(defmacro quot (list)
  `(identity ',(loop for l in list
                     collect `(quote ,l))))
(defun make-wash (&rest fns)
  (defun wash (type code)
    (funcall (apply #'partial-1 type fns) code)))
#|(defwashers
((csv (csv) csv) ;; first washers
(xml (xml)
(cl-ppcre:regex-replace "&.*?;" xml " ")))
((csv (csv) (mapcar #'(lambda (s) (splist "," s)) ;; second washer
(splist (format nil "~%") csv)))
(xml (xml) xml)))|#
(defun types (washers)
  "Generate a type list from the function names of the washers."
  (loop for w in washers
        collect (intern (symbol-name (car w)) (symbol-package :bogus))))
(defun genlist (len)
  (loop repeat len collect (gensym)))
(defmacro defwashers (&rest washers)
  (let ((gensyms (loop for x from 1 to (length washers)
                       collect (symb 'wash x))))
    `(progn ,@(loop for g in gensyms
                    for w in washers
                    collect `(defwasher ,g ,(types w) ,w))
            (apply #'make-wash ',(reverse gensyms)))))
#|(let ((,wash1 (gensym))
(,wash2 (gensym))) (defun wash (type code)
(funcall (compose #:Gwash1 #:Gwash2...)))
(defun #:Gwash1 (...)...)
(defun #:Gwash2 (...) ...))|#
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
