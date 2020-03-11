# Purpose
Downloads audiobooks and images from [librivox](librivox.org), then makes a video and uploads it to youtube.
# Requirements:
Foreign command line applications:
`mp3info`
`ffmpeg`
`bash`
In your lisp:
`Quicklisp`
`The LATEST CL+SSL, from the source repo. Quicklisp might not work.`
If CL+SSL doesn't work,
`curl`
command line application can be used as a workaround.

A google console secret must be added to `src/client_secrets.json`
