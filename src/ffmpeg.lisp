(in-package :librivox)
(defvar *bash-output* *debug-io*)
(defparameter *seconds* 0 "The time in seconds of the entire book.")
(defvar *downloads-dir* "/home/jose/common-lisp/librivox/downloads/")
(defun run-lines (&rest code-strings)
  (mapcar #'(lambda (x) (run-program (format nil x *downloads-dir*) :output *bash-output*))
	  code-strings))
(defun nth-digit (digit number base)
  (if (= 0 digit)
      (mod number base)
      (nth-digit (1- digit) (floor number base) base)))
(defun hours (seconds)
  (/ (- seconds (* 60 (minutes seconds)) (seconds seconds))
     60 60))
(defun minutes (seconds)
  (nth-digit 1 seconds 60))
(defun seconds (seconds)
  (nth-digit 0 seconds 60))

(defun convert (image-pathname output-pathname)
  "Convert the zip to video."
  ;; ~A for the download directory, ~:*~A after the first.
  (let ((output-pathname (merge-pathnames output-pathname *downloads-dir*)))
    (unwind-protect (let ((image-pathname (merge-pathnames image-pathname *downloads-dir*)))
		      (run-lines "unzip ~A*.zip -d ~:*~A"
				 "for f in ~A*.mp3; do echo \"file '$f'\" >> ~:*~Aconcat.txt; done"
				 "ffmpeg -f concat -safe 0 -i ~Aconcat.txt -c copy ~:*~Aoutput1.mp3")
		      (setf *seconds* (parse-integer (run-program (format nil "mp3info -p \"%S\" ~Aoutput1.mp3" *downloads-dir*) ;
								  :output 'string)
						     :junk-allowed t))
		      (run-lines (format nil "ffmpeg -loop 1 -i ~A -t ~D:~D:~D -c mjpeg ~~Atemp.mp4" ;
					 image-pathname
					 (hours *seconds*)
					 (minutes *seconds*)
					 (seconds *seconds*)))
	       
		      (run-lines (format nil"ffmpeg -i ~~Atemp.mp4 -i ~~:*~~Aoutput1.mp3 -c copy ~~:*~~Aconverted/~A"
					 output-pathname)
				 "rm ~Atemp.mp4"
				 "rm ~A*.mp3"
				 "rm ~Aconcat.txt"))
      (run-lines "rm ~Atemp.mp4"
		 "rm ~A*.mp3"
		 "rm ~Aconcat.txt"))))
;;; Example input: (convert "image.jpg" "final-test-probably.mp4")

