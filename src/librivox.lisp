(in-package :librivox)
(defun start ()
  "Just start everything: download, convert, upload."
  (csv-expand (csv-update) :forward t)
  (convert-all))

(defun csv-update ()
  "Get a new CSV to keep up with recent audiobook publishing.")
(defun csv-expand (&key forward)
  "Convert a CSV of librivox audiobooks into a directory tree filled with files
to be used in the downloading and conversion portions. `FORWARD' determines if
the directory tree is being made fresh -> nil or just updated -> t. "
  )
