(in-package #:query)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun group (source n)
    (labels ((rec (source acc)
               (let ((rest (nthcdr n source)))
                 (if (consp rest)
                     (rec rest (cons
                                (subseq source 0 n)
                                acc))
                     (nreverse
                      (cons source acc))))))
      (if source (rec source nil) nil))))
(defun shuffle (x y &key (key-x #'identity) (key-y #'identity))
  "Interpolate lists x and y with the first item being from x."
  (cond ((null x) y)
        ((null y) x)
        (t (list* (funcall key-x (car x)) (funcall key-y (car y))
                  (shuffle (cdr x) (cdr y))))))
(defun interpol (obj lst &key (key #'identity))
  "Intersperse an object in a list."
  (shuffle lst (loop for #1=#.(gensym) in (cdr lst)
		     collect obj)
           :key-x key))
(defmacro defcollect (name collector argn)
  "Collect a bunch of args into multiple invocations of a funcion. One example
of this is setf/setq: (setf a b c d) -> (setf a b) (setf c d)"
  (with-gensyms (x args)
    `(defmacro ,name (&rest ,args)
       `(progn ,@(loop for ,x in (group ,args ,argn)
                       collect (cons ',collector ,x))))))
(defmacro defquery (before name after)
  `(defun ,name (name val)
     (concatenate 'string
                  ,before
                  (string-downcase (symbol-name name))
                  ,after
                  val)))
(defcollect defqueries defquery 3)
(defqueries "&" & "="
  "?" ? "="
  "{" { "}")

(defun csvs (strlist)
  "Comma separate a list of strings."
  (apply #'concatenate 'string (interpol "," strlist)))
