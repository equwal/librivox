(in-package :librivox)
(defvar *downloads-dir* "/home/jose/common-lisp/librivox/downloads/")
(defun run-line* (code-string &optional (output *bash-output*))
  (run-line (format nil code-string *downloads-dir*) :output output))
(defun run-line*-integer-output (code-string)
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
  #+linux (run-line*-integer-output "grep -c ^processor /proc/cpuinfo~*")
  #-linux 1)
(defun convert (&key image output zip)
  "Convert the zip to video."
  ;; ~A for the download directory, ~:*~A after the first.
  ;; KLUDGE: if there is more than one zip 
  ;; KLUDGE: rm returns error code 1 for some reason, causing continuations.
  ;;         I would rather log those and automatically continue.
  (ignore-errors (let ((output (merge-pathnames output *downloads-dir*))
		       (image (merge-pathnames image *downloads-dir*))
		       (zip (merge-pathnames zip *downloads-dir*)))
		   (run-line* (format nil "unzip ~A -d ~~A" zip))
		   (run-line* "for f in ~A*.mp3; do echo \"file '$f'\" >> ~:*~Aconcat.txt; done")
		   (run-line* "ffmpeg -f concat -safe 0 -i ~Aconcat.txt -c copy ~:*~Aoutput1.mp3")
		   (let ((seconds (run-line*-integer-output "mp3info -p \"%S\" ~Aoutput1.mp3")))
		     (run-line* (format nil "ffmpeg -loop 1 -i ~A -t ~D:~D:~D -c mjpeg ~~Atemp.mp4" ;
					image
					(hours seconds)
					(minutes seconds)
					(seconds seconds)))
		     (run-line* (format nil "ffmpeg -i ~~Atemp.mp4 -i ~~:*~~Aoutput1.mp3 -c copy ~A"
				       output))
		     (run-line* "rm ~Atemp.mp4")
		     (run-line* "rm ~A*.mp3")
		     (run-line* "rm ~Aconcat.txt")))))
;;; Example:
#|(bash:convert :image "abe_mawruss_1807.jpg" 
	      :output "final-test-probably1.mp4"
	      :zip "abeandmawruss_1807_librivox.zip")|#
