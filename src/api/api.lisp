(in-package :api)
(defun next-query (s v &key first)
  (if (eql s :fields)
      (if first
          (? s ({ v))
          (& s ({ v)))
      (if first
          (? s v)
          (& s v))))
(defun query (format &rest selector-value-pairs)
  "Produce a query to download the librivox content."
  (concat 'string
          *base-url*
          (next-query (car selector-value-pairs)
                      (cadr selector-value-pairs)
                      :first t)
          (& :format format)
          (mapcar #'(lambda (p)
                      (next-query (car p) (cadr p)))
                  (cdr (group selector-value-pairs 2)))))
(defun syms->strs (syms)
  (mapcar #'string-downcase (mapcar #'symbol-name syms)))
(defmacro awhen (test then)
  `(let ((it ,test))
     (when it ,then)))
(defun pull (since)
  (awhen (wget (query :xml
		      :since since
		      :fields (syms->strs
			       (set-difference
				'(last_name first_name)
				(cons 'authors *fields*)))))
    (if (match "<error>.*</error>" it)
	(values nil "Failed: error in XML.")
	(funcall (wash :xml) it))))
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
(defun find-tag (tag clean-xml)
  (if (or (atom clean-xml) (null clean-xml))
      nil
      (if (and (stringp (car clean-xml)) (string-equal tag (car clean-xml)))
          (cadr clean-xml)
          )))
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
(defmacro chain (args &rest transforms)
  (let ((lambdas (loop for x in transforms
                       collect `(lambda ,args ,x))))
    `(lambda ,args
       (funcall (compose ,@(reverse lambdas)) ,@args))))

(defun wash (type)
  (case type
    (:csv (mapcar #'(lambda (s) (splist "," s))
                  (splist (format nil "~%") csv)) string)
    (:xml (chain (string)
                 (labels ((self (string)
                            (let ((match (match "&.*?;" (force-str string))))
                              (if match
                                  (self (escape match (force-escape string)))
                                  string))))
                   (self string))
                 (cons (escape-tbl (force-escape string))
                       (xmls:parse (force-str string)
                                   :compress-whitespace nil))
                 (let ((tbl (car string))
                       (string (cdr string)))
                   (mapatoms #'(lambda (str)
                                 (when str
                                   (escape-str (unescape (make-escape
                                                          :tbl tbl
                                                          :str str)))))
                             string))
                 (order-books (mapcar #'destructure-book
                                      (cdadr (tree-remove nil string))))))))
