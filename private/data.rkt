#lang racket/base

(require ffi/unsafe
         (only-in racket/contract or/c listof))

(provide (all-defined-out))

(struct timespec (sec nsec))

(struct attr (ino size blocks atime mtime ctime kind perm nlink uid gid rdev))

(define _xattr_t
  (_enum '(XATTR_DEFAULT = 0
           XATTR_CREATE  = 1
           XATTR_REPLACE = 2)))

(define (xattr-flag? f)
  (member f '(XATTR_DEFAULT XATTR_CREATE XATTR_REPLACE)))

(define _mode_t
  (_bitmask '(S_IFSOCK = #o140000
              S_IFLNK  = #o120000
              S_IFREG  = #o100000
              S_IFBLK  = #o060000
              S_IFDIR  = #o040000
              S_IFCHR  = #o020000
              S_IFIFO  = #o010000
              S_ISUID  = #o004000
              S_ISGID  = #o002000
              S_ISVTX  = #o001000
              S_IRUSR  = #o000400
              S_IWUSR  = #o000200
              S_IXUSR  = #o000100
              S_IRGRP  = #o000040
              S_IWGRP  = #o000020
              S_IXGRP  = #o000010
              S_IROTH  = #o000004
              S_IWOTH  = #o000002
              S_IXOTH  = #o000001)
            _uint32))

(define (mode-flag? m)
  (member m '(S_ISUID S_ISGID S_ISVTX S_IRUSR S_IWUSR S_IXUSR S_IRGRP S_IWGRP S_IXGRP S_IROTH S_IWOTH S_IXOTH)))

(define mode/c (or/c mode-flag? (listof mode-flag?)))

(define (filetype? t)
  (member t '(S_IFSOCK S_IFLNK S_IFREG S_IFBLK S_IFDIR S_IFCHR S_IFIFO)))

(define _flag_t
  (_bitmask '(O_RDONLY    = #o00000000
              O_WRONLY    = #o00000001
              O_RDWR      = #o00000002
              O_CLOEXEC   = #o02000000
              O_CREAT     = #o00000100
              O_DIRECTORY = #o00200000
              O_EXCL      = #o00000200
              O_NOCTTY    = #o00000400
              O_NOFOLLOW  = #o00400000
              O_TMPFILE   = #o20000000
              O_TRUNC     = #o00001000
              O_APPEND    = #o00002000
              O_NONBLOCK  = #o00004000
              O_PATH      = #o10000000
              O_DSYNC     = #o00010000
              O_DIRECT    = #o00040000
              O_LARGEFILE = #o00100000
              O_NOATIME   = #o01000000)
            _uint32))

(define (flag? f)
  (member f '(O_RDONLY O_WRONLY O_RDWR O_CLOEXEC O_CREAT O_DIRECTORY O_EXCL O_NOCTTY O_NOFOLLOW
              O_TMPFILE O_TRUNC O_APPEND O_NONBLOCK O_PATH O_DSYNC O_DIRECT O_LARGEFILE O_NOATIME)))

(define flags/c (listof flag?))

(define _lock_type
  (_enum '(F_RDLCK = 5
           F_WRLCK = 6
           F_UNLCK = 7)
         _short))

(define (lock-type? t)
  (member t '(F_RDLCK F_WRLCK F_UNLCK)))

(define _lock_whence
  (_enum '(SEEK_SET = 0
           SEEK_CUR = 1
           SEEK_END = 2)
         _short))

(define (lock-whence? w)
  (member w '(SEEK_SET SEEK_CUR SEEK_END)))

(define (uint64? n)
  (and (exact-nonnegative-integer? n)
       (<= (integer-length n) 64)))

(define (uint32? n)
  (and (exact-nonnegative-integer? n)
       (<= (integer-length n) 32)))

(define (or-flags f1 f2)
  (cond
    [(and (symbol? f1) (symbol? f2)) (list f1 f2)]
    [(symbol? f1) (cons f1 f2)]
    [(symbol? f2) (cons f2 f1)]
    [else (append f1 f2)]))
