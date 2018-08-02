(in-package :librivox)
;; As far as I can tell counting processor cores is a common way of determining
;; max threads. FFMPEG does it automatically.
(defparameter *max-threads* #+multithreading (processor-cores)
	      #- multithreading 1)
(defvar *url* "https://librivox.org/rss/latest_releases")

(if (= 1 *max-threads*)
    ;; No multithreading: One book at a time.
    (parse-feed (http-request *url*))
    ;; Multithreading: One thread per book method.
    ())
