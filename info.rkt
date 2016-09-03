#lang info
(define collection "fuse")
(define deps '("base"
               "rackunit-lib"))
(define build-deps '("scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/fuse.scrbl" ())))
(define pkg-desc "A Racket library for writing FUSE userspace filesystems")
(define version "0.1")
(define pkg-authors '(Scott Moore))
