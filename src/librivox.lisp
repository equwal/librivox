(in-package :librivox)
;; As far as I can tell counting processor cores is a common way of determining
;; max threads. FFMPEG does it automatically.
(defparameter *max-threads* #+multithreading (processor-cores)
	      #- multithreading 1)
(defun processor-cores ()
  #+linux (run-line*-integer-output "grep -c ^processor /proc/cpuinfo~*")
  #-linux 1)
