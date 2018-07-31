(in-package :bash)
(defvar *bash-output* *debug-io*)
(defvar *downloads-dir* "/home/jose/common-lisp/librivox/downloads/")
(defun run-line (code-string &optional (output *bash-output*))
  (run-program (format nil code-string *downloads-dir*) :output output))
(defun run-line-integer-output (code-string)
  (parse-integer (run-program (format nil code-string *downloads-dir*)
			      :output 'string)
		 :junk-allowed t))
(defun hours (seconds)
  (/ (- seconds (* 60 (minutes seconds)) (seconds seconds))
     60 60))
(defun minutes (seconds)
  (nth-digit 1 seconds 60))
(defun seconds (seconds)
  (nth-digit 0 seconds 60))
(defun processor-cores ()
  #+linux (run-line-integer-output "grep -c ^processor /proc/cpuinfo~*")
  #-linux 1)
(defun convert (image-filename output-filename zip-filename)
  "Convert the zip to video."
  ;; ~A for the download directory, ~:*~A after the first.
  ;; KLUDGE: if there is more than one zip 
  ;; KLUDGE: rm returns error code 1 for some reason, causing continuations.
  ;;         I would rather log those and automatically continue.
  (ignore-errors (let ((output-filename (merge-pathnames output-filename *downloads-dir*))
		       (image-filename (merge-pathnames image-filename *downloads-dir*))
		       (zip-filename (merge-pathnames zip-filename *downloads-dir*)))
		   (run-line (format nil "unzip ~A -d ~~A" zip-filename))
		   (run-line "for f in ~A*.mp3; do echo \"file '$f'\" >> ~:*~Aconcat.txt; done")
		   (run-line "ffmpeg -f concat -safe 0 -i ~Aconcat.txt -c copy ~:*~Aoutput1.mp3")
		   (let ((seconds (run-line-integer-output "mp3info -p \"%S\" ~Aoutput1.mp3")))
		     (run-line (format nil "ffmpeg -loop 1 -i ~A -t ~D:~D:~D -c mjpeg ~~Atemp.mp4" ;
					image-filename
					(hours seconds)
					(minutes seconds)
					(seconds seconds)))
		     (run-line (format nil "ffmpeg -i ~~Atemp.mp4 -i ~~:*~~Aoutput1.mp3 -c copy ~A"
				       output-filename))
		     (run-line "rm ~Atemp.mp4")
		     (run-line "rm ~A*.mp3")
		     (run-line (format nil "rm ~A ~A~~*" zip-filename image-filename))
		     (run-line "rm ~Aconcat.txt")))))
;;; (convert "abe_mawruss_1807.jpg" "final-test-probably1.mp4" "abeandmawruss_1807_librivox.zip")

