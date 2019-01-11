;;;; Example request:
;;;; python upload_video.py --file="/tmp/test_video_file.flv"
;;;; --title="Summer vacation in California"
;;;; --description="Had fun surfing in Santa Cruz"
;;;; --keywords="surfing,Santa Cruz"
;;;; --category="22"
;;;; --privacyStatus="private"

(in-package #:librivox)

(defun pair-keywords (keywords)
  "Generate a proper bash call for paired keywords."
  (labels ((inner (keywords first acc)
             (if (null keywords)
                 acc
                 (if first
                     (inner (cdr keywords) nil (car keywords))
                     (inner (cdr keywords) nil (concatenate 'string
                                                            ","
                                                            (car keywords)))))))
    (inner keywords t "")))

(defun upload (&key file
                 title
                 description
                 keywords
                 category
                 privacy-status)
  (run-line (concatenate 'string
                         "--file=\""
                         *downloads-dir*
                         file
                         "\" "
                         "--title=\""
                         title
                         "\" "
                         "--description=\""
                         description
                         "\" "
                         "--keywords=\""
                         (pair-keywords keywords)
                         "\" "
                         "--category=\""
                         category
                         "\" "
                         "--privacyStatus=\""
                         privacy-status
                         "\"")))
