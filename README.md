racket-fuse
====

A Racket library for implementing Filesystems in Userspace (FUSE).

This package implements communication routines for interfacing with the Linux FUSE kernel driver.
Client programs can implement a userspace filesystem by providing a collection of functions that
implement filesystem operations.

Currently, racket-fuse depends on the libfuse C library to mount a FUSE filesystem. However, the
communication protocol with the kernel and filesystem API does not reuse libfuse functionality.

Documentation is available at https://docs.racket-lang.org/fuse/index.html.
