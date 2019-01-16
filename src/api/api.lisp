(in-package :api)
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

(defun previous-time (path)
  (with-open-file (s path)
    (parse-integer (read-line s nil nil))))
(defun save-time (path time)
  (with-open-file (s path :direction :output
                          :if-exists :overwrite
                          :if-does-not-exist :create)
    (prin1 time s)))

(defun update (path &key (forward t))
  (let ((before (get-epoch-time)))
    (mapfns (path forward) #'update-file #'expand)
    (save-time *last-update* before)))
(defun xml-tags (parsed)
  (labels ((kludge-inner (lst gensym)
             (if (flat lst)
                 (car lst)
                 (if (atom lst)
                     gensym
                     (list (kludge-inner (car lst) gensym)
                           (kludge-inner (cdr lst) gensym))))))
    (let ((g (gensym)))
      (remove-duplicates (remove-if #'(lambda (s) (eql s g))
                                    (flatten (kludge-inner parsed g)))
                         :test #'string=))))
(defun tree-remove (val tree &key (test #'eql))
  (if (null tree)
      nil
      (if (consp (car tree))
          (cons (tree-remove val (car tree) :test test)
                (tree-remove val (cdr tree) :test test))
          (if (funcall test val (car tree))
              (tree-remove val (cdr tree) :test test)
              (cons (car tree)
                    (tree-remove val (cdr tree) :test test))))))
(defun destructure-book (book)
  ;; How does this mess work? (and how did I make it work?)
  (labels ((destructure-authors (authors)
             (remove nil (list (list #1="first_name" (find-tag #1# authors))
                               (list #2="last_name" (find-tag #2# authors)))))
           (kludge (book)
             (if (atom book)
                 book
                 (if (stringp (car book))
                     (if (string-equal (car book) "authors")
                         (destructure-authors book)
                         (if (stringp (cadr book))
                             book
                             (cons (kludge (car book))
                                   (kludge (cdr book)))))
                     (cons (kludge (car book))
                           (kludge (cdr book)))))))
    (group (flatten (cdr (kludge book))) 2)))
(defun order-books (books)
  (let* ((header (sort (mapcar #'string-downcase (mapcar #'symbol-name
                                                         *fields*))
                       #'string<)))
    (cons header
          (mapcar #'(lambda (book)
                      (apply-map (gen-map #'string< (mapcar #'car book))
                                 (mapcar #'cadr book)))
                  books))))
(washer
 ;; Applied first to last. Use washers like an onion/filter.
 (xml (raw)
      (let ((match (match "&.*?;" (force-str raw))))
        (if match
            (wash1 :xml (escape match (force-escape raw)))
            raw)))
 (xml (escaped) (cons (escape-tbl (force-escape escaped))
                      (xmls:parse (force-str escaped)
                                  :compress-whitespace nil)))
 (xml (parsed) (let ((tbl (car parsed))
                     (xml (cdr parsed)))
                 (mapatoms #'(lambda (str)
                               (when str
                                 (escape-str (unescape (make-escape
                                                        :tbl tbl
                                                        :str str)))))
                           xml)))
 (xml (unescaped) (order-books (mapcar #'destructure-book
                                       (cdadr (tree-remove nil unescaped)))))
 (csv (csv) (mapcar #'(lambda (s) (splist "," s))
                    (splist (format nil "~%") csv)))
 (csv (csv) (order-fields csv)))
