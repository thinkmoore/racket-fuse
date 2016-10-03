#lang racket/base

(require racket/match
         racket/list
         fuse)

(define (lookup #:nodeid nodeid #:name name #:reply reply-entry #:error error)
  (if (and (= nodeid 1) (equal? name (string->path "hello.txt")))
      (reply-entry #:generation 0 #:entry-valid (timespec 1 0) #:attr-valid (timespec 1 0)
                   #:inode 2 #:rdev 0 #:size 13 #:blocks 1
                   #:atime (timespec 1381237736 0) #:mtime (timespec 1381237736 0)
                   #:ctime (timespec 1381237736 0) #:kind 'S_IFREG
                   #:perm '(S_IRUSR S_IWUSR S_IRGRP S_IROTH)
                   #:nlink 1 #:uid 1000 #:gid 1000)
      (error 'ENOENT)))

(define (getattr #:nodeid nodeid #:info info #:reply reply-attr #:error error)
  (match nodeid
    [1 (reply-attr #:attr-valid (timespec 1 0)
                   #:inode 1 #:rdev 0 #:size 0 #:blocks 0
                   #:atime (timespec 1381237736 0) #:mtime (timespec 1381237736 0)
                   #:ctime (timespec 1381237736 0) #:kind 'S_IFDIR
                   #:perm '(S_IRUSR S_IWUSR S_IXUSR S_IRGRP S_IXGRP S_IROTH S_IXOTH)
                   #:nlink 1 #:uid 1000 #:gid 1000)]
    [2 (reply-attr #:attr-valid (timespec 1 0)
                   #:inode 2 #:rdev 0 #:size 13 #:blocks 1
                   #:atime (timespec 1381237736 0) #:mtime (timespec 1381237736 0)
                   #:ctime (timespec 1381237736 0) #:kind 'S_IFREG
                   #:perm '(S_IRUSR S_IWUSR S_IRGRP S_IROTH)
                   #:nlink 1 #:uid 1000 #:gid 10000)]
    [_ (error 'ENOENT)]))

(define (readdir #:nodeid nodeid #:info info #:offset offset #:add reply-add #:reply reply-done #:error error)
  (if (= nodeid 1)
      (begin
        (when (= offset 0)
          (reply-add #:inode 1 #:offset 0 #:kind 'S_IFDIR #:name (string->path "."))
          (reply-add #:inode 1 #:offset 1 #:kind 'S_IFDIR #:name (string->path ".."))
          (reply-add #:inode 1 #:offset 2 #:kind 'S_IFREG #:name (string->path "hello.txt")))
        (reply-done))
      (error 'ENOENT)))

(define (open #:nodeid nodeid #:flags flags #:reply reply-open #:error error)
  (if (= nodeid 2)
      (reply-open #:info #f #:flags empty)
      (error 'EISDIR)))

(define (read #:nodeid nodeid #:info info #:offset offset #:size size #:flags flags #:lockowner lockowner #:reply reply-data #:error error)
  (reply-data (string->bytes/utf-8 "Hello world!\n")))

(define hellofs (make-filesystem #:lookup lookup #:getattr getattr #:readdir readdir #:open open #:read read))

(module+ main
  (require racket/runtime-path)

  (define-runtime-path tmp "tmp")
  (unless (directory-exists? tmp)
    (make-directory tmp))
  (mount-filesystem hellofs tmp (list "default_permissions" "large_read" "direct_io" "hard_remove")))
