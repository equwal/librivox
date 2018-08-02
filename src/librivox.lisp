(in-package :librivox)
;; As far as I can tell counting processor cores is a common way of determining
;; max threads. FFMPEG does it automatically.
(defparameter *max-threads* #+multithreading (processor-cores)
	      #- multithreading 1)
(defvar *url* "https://librivox.org/rss/latest_releases")

(defun main-loop ()
    (if (= 1 *max-threads*)
	;; No multithreading: One book at a time.
	(parse-feed (http-request *url*))
	;; Multithreading: One thread per book method.
	()))
(defun rate (specification actual &optional (test #'eql))
  (if (or (null specification) (null actual))
      0
      (if (and (consp (car specification)) (consp (car actual)))
	  (rate (car specification) (car actual) test)
	  (+ (if (funcall test (car specification) (car actual)) 1 0)
	     (rate (cdr specification) (cdr actual) test)))))
