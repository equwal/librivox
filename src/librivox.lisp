(in-package :librivox)

(defun start (csv-input)
  "Just start everything: download, convert, upload."
  (download)
  (convert)
  (upload))
