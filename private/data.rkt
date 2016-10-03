#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     syntax/parse/experimental/template)
         syntax/parse
         ffi/unsafe
         (only-in racket/contract or/c listof))

(struct timespec (sec nsec))

(define (timespec-now)
  (let* ([now (current-inexact-milliseconds)]
         [seconds (truncate (/ now 1000))]
         [nanoseconds (* (- now (* seconds 1000)) 1000000)])
    (timespec seconds nanoseconds)))

(provide (struct-out timespec) timespec-now)

(struct attr (ino size blocks atime mtime ctime kind perm nlink uid gid rdev))

(provide (struct-out attr))

(begin-for-syntax
  (define-splicing-syntax-class bit-definer
    #:literals (=)
    (pattern (~seq name:id = val:nat))
    (pattern name:id)))

(define-syntax (define-enum stx)
  (syntax-parse stx
    [(_ name:id (def:bit-definer ...) type:id)
     (with-syntax ([cname (format-id #'name "_~a" #'name)]
                   [pred  (format-id #'name "~a?" #'name)])
       (template
        (begin
          (define cname (_enum '((?@ . def) ...) type))
          (define (pred v)
            (member v '(def.name ...))))))]))

(define-syntax (define-bitmask stx)
  (syntax-parse stx
    [(_ name:id (def:bit-definer ...) type:id)
     (with-syntax ([cname (format-id #'name "_~a" #'name)]
                   [pred  (format-id #'name "~a?" #'name)])
       (template
        (begin
          (define cname (_bitmask '((?@ . def) ...) type))
          (define (pred v)
            (member v '(def.name ...))))))]))

(define-enum xattr-op
  (XATTR_DEFAULT = 0
   XATTR_CREATE
   XATTR_REPLACE)
  _uint32)

(provide xattr-op?)

(define-bitmask mode
  (S_IFSOCK = #o140000
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
  _uint32)

(define (perm? m)
  (member m '(S_ISUID S_ISGID S_ISVTX S_IRUSR S_IWUSR S_IXUSR S_IRGRP S_IWGRP S_IXGRP S_IROTH S_IWOTH S_IXOTH)))

(define perm/c (or/c perm? (listof perm?)))

(define (filetype? t)
  (member t '(S_IFSOCK S_IFLNK S_IFREG S_IFBLK S_IFDIR S_IFCHR S_IFIFO)))

(define (mode->perm mode)
  (if (list? mode)
      (filter perm? mode)
      (if (perm? mode) mode #f)))

(define (mode->filetype mode)
  (if (list? mode)
      (filter filetype? mode)
      (if (filetype? mode) mode #f)))

(provide mode? perm? perm/c filetype? mode->perm mode->filetype)

(define-bitmask oflag
  (O_RDONLY    = #o00000000
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
  _uint32)

(define oflags/c (listof oflag?))

(provide oflags/c oflag?)

(define-enum lock-type
  (F_RDLCK = 5
   F_WRLCK
   F_UNLCK)
  _short)

(provide lock-type?)

(define-enum lock-whence
  (SEEK_SET = 0
   SEEK_CUR
   SEEK_END)
  _short)

(provide lock-whence?)

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

(define (check-flag flags flag)
  (cond
    [(eq? flags flag) #t]
    [(and (list? flags) (member flag flags)) #t]
    [else #f]))

(provide uint64? uint32? or-flags check-flag)

(define-bitmask setattr-valid
  (FATTR_MODE
   FATTR_UID
   FATTR_GID
   FATTR_SIZE
   FATTR_ATIME
   FATTR_MTIME
   FATTR_FH
   FATTR_ATIME_NOW
   FATTR_MTIME_NOW
   FATTR_LOCKOWNER
   FATTR_CTIME)
  _uint32)

(define-bitmask open-out-flags
  (FOPEN_DIRECT_IO FOPEN_KEEP_CACHE FOPEN_NONSEEKABLE)
  _uint32)

(provide open-out-flags?)

(define-bitmask fuse-flags
  (FUSE_ASYNC_READ
   FUSE_POSIX_LOCKS
   FUSE_FILE_OPS
   FUSE_ATOMIC_O_TRUNC
   FUSE_EXPORT_SUPPORT
   FUSE_BIG_WRITES
   FUSE_DONT_MASK
   FUSE_SPLICE_WRITE
   FUSE_SPLICE_MOVE
   FUSE_SPLICE_READ
   FUSE_FLOCK_LOCKS
   FUSE_HAS_IOCTL_DIR
   FUSE_AUTO_INVAL_DATA
   FUSE_DO_READDIRPLUS
   FUSE_ASYNC_DIO
   FUSE_WRITEBACK_CACHE
   FUSE_NO_OPEN_SUPPORT)
  _uint32)

(define-bitmask write-flags
  (FUSE_WRITE_CACHE FUSE_WRITE_LOCKOWNER)
  _uint32)

(define-bitmask read-flags
  (FUSE_READ_LOCKOWNER)
  _uint32)

(define-bitmask ioctl-flags
  (FUSE_IOCTL_COMPAT
   FUSE_IOCTL_UNRESTRICTED
   FUSE_IOCTL_RETRY
   FUSE_IOCTL_32BIT
   FUSE_IOCTL_DIR)
  _uint32)

(define-bitmask poll-flags
  (FUSE_POLL_SCHEDULE_NOTIFY)
  _uint32)

(define-bitmask getattr-flags
  (FUSE_GETATTR_FH)
  _uint32)

(define-bitmask release-flags
  (FUSE_RELEASE_FLUSH FUSE_RELEASE_FLOCK_UNLOCK)
  _uint32)

(define-bitmask lock-flags
  (FUSE_LK_FLOCK)
  _uint32)

(define-bitmask rename-flags
  (RENAME_EXCHANGE RENAME_NOREPLACE RENAME_WHITEOUT)
  _uint32)

(define-bitmask fsync-flags
  (DATASYNC)
  _uint32)

(define-enum opcode
  (FUSE_LOOKUP = 1
   FUSE_FORGET
   FUSE_GETATTR
   FUSE_SETATTR
   FUSE_READLINK
   FUSE_SYMLINK
   FUSE_MKNOD = 8
   FUSE_MKDIR
   FUSE_UNLINK
   FUSE_RMDIR
   FUSE_RENAME
   FUSE_LINK
   FUSE_OPEN
   FUSE_READ
   FUSE_WRITE
   FUSE_STATFS
   FUSE_RELEASE
   FUSE_FSYNC = 20
   FUSE_SETXATTR
   FUSE_GETXATTR
   FUSE_LISTXATTR
   FUSE_REMOVEXATTR
   FUSE_FLUSH
   FUSE_INIT
   FUSE_OPENDIR
   FUSE_READDIR
   FUSE_RELEASEDIR
   FUSE_FSYNCDIR
   FUSE_GETLK
   FUSE_SETLK
   FUSE_SETLKW
   FUSE_ACCESS
   FUSE_CREATE
   FUSE_INTERRUPT
   FUSE_BMAP
   FUSE_DESTROY
   FUSE_IOCTL
   FUSE_POLL
   FUSE_NOTIFY_REPLY
   FUSE_BATCH_FORGET
   FUSE_FALLOCATE
   FUSE_READDIRPLUS
   FUSE_RENAME2)
  _uint32)

(define-enum notify_code
  (FUSE_NOTIFY_POLL = 1
   FUSE_NOTIFY_INVAL_INODE
   FUSE_NOTIFY_INVAL_ENTRY
   FUSE_NOTIFY_STORE
   FUSE_NOTIFY_RETRIEVE
   FUSE_NOTIFY_DELETE
   FUSE_NOTIFY_CODE_MAX)
  _uint32)

(define-syntax (define-message stx)
  (define (generate-accessors name fields)
    (map (lambda (field) (format-id name "~a-~a" name field)) fields))
  (define (generate-offsets name fields)
    (map (lambda (field) (format-id name "~a-inner-~a-offset" name field)) fields))
  (syntax-parse stx
    [(_ name:id ([field:id ctype:id] ...))
     (with-syntax
       ([cname                (format-id #'name "_~a" #'name)]
        [cname-inner          (format-id #'name "_~a-inner" #'name)]
        [constructor          (format-id #'name "make-~a-inner" #'name)]
        [(field-accessor ...) (generate-accessors #'name (syntax->list #'(field ...)))]
        [(cfield-offset ...)  (generate-offsets #'name (syntax->list #'(field ...)))]
        [decode               (format-id #'name "decode-~a" #'name)]
        [skip                 (format-id #'name "skip-~a" #'name)])
       #'(begin
           (struct name (field ...))
           (define-cstruct cname-inner
             ([field ctype] ...)
             #:define-unsafe)
           (define cname
             (make-ctype cname-inner
                         (lambda (r) (constructor (field-accessor r) ...))
                         (lambda (c) (name (ptr-ref (ptr-add c cfield-offset) ctype) ...))))
           (define (decode cptr)
             (ptr-ref cptr cname))
           (define (skip cptr)
             (ptr-add cptr 1 cname))
           (provide (struct-out name) cname decode skip)))]))

(define-message fuse_entry_out
  ([nodeid          _uint64]
   [generation      _uint64]
   [ent_valid       _uint64]
   [attr_valid      _uint64]
   [ent_valid_nsec  _uint32]
   [atrr_valid_nsec _uint32]
   [ino             _uint64]
   [size            _uint64]
   [blocks          _uint64]
   [atime           _uint64]
   [mtime           _uint64]
   [ctime           _uint64]
   [atimensec       _uint32]
   [mtimensec       _uint32]
   [ctimensec       _uint32]
   [mode            _mode]
   [nlink           _uint32]
   [uid             _uint32]
   [gid             _uint32]
   [rdev            _uint32]
   [blksize         _uint32]
   [padding         _uint32]))

(define-message fuse_forget_in
  ([nlookup _uint64]))

(define-message fuse_forget_one
  ([nodeid  _uint64]
   [nlookup _uint64]))

(define-message fuse_batch_forget_in
  ([count   _uint32]
   [dummy   _uint32]))

(define-message fuse_getattr_in
  ([flags   _getattr-flags]
   [dummy   _uint32]
   [user    _racket]))

(define-message fuse_attr_out
  ([attr_valid      _uint64]
   [attr_valid_nsec _uint32]
   [dummy           _uint32]
   [ino             _uint64]
   [size            _uint64]
   [blocks          _uint64]
   [atime           _uint64]
   [mtime           _uint64]
   [ctime           _uint64]
   [atimensec       _uint32]
   [mtimensec       _uint32]
   [ctimensec       _uint32]
   [mode              _mode]
   [nlink           _uint32]
   [uid             _uint32]
   [gid             _uint32]
   [rdev            _uint32]
   [blksize         _uint32]
   [padding         _uint32]))

(define-message fuse_mknod_in
  ([mode    _mode]
   [rdev  _uint32]
   [umask   _mode]
   [dummy _uint32]))


(define-message fuse_mkdir_in
  ([mode  _mode]
   [umask _mode]))

(define-message fuse_rename_in
  ([newdir _uint64]))

(define-message fuse_rename2-in
  ([newdir        _uint64]
   [flags   _rename-flags]
   [padding       _uint32]))

(define-message fuse_link_in
  ([oldnodeid _uint64]))

(define-message fuse_setattr_in
  ([valid    _setattr-valid]
   [padding         _uint32]
   [user            _racket]
   [size            _uint64]
   [unused1         _uint64]
   [atime           _uint64]
   [mtime           _uint64]
   [ctime           _uint64]
   [atimensec       _uint32]
   [mtimensec       _uint32]
   [ctimensec       _uint32]
   [mode              _mode]
   [unused4         _uint32]
   [uid             _uint32]
   [gid             _uint32]
   [unused5         _uint32]))

(define-message fuse_open_in
  ([flags   _oflag]
   [unused _uint32]))

(define-message fuse_create_in
  ([flags    _oflag]
   [mode      _mode]
   [umask     _mode]
   [padding _uint32]))

(define-message fuse_open_out
  ([user          _racket]
   [flags _open-out-flags]
   [padding       _uint32]))

(define-message fuse_release_in
  ([user          _racket]
   [flags          _oflag]
   [rflags _release-flags]
   [lckowner      _uint64]))

(define-message fuse_flush_in
  ([user     _racket]
   [unused   _uint32]
   [padding  _uint32]
   [lckowner _uint64]))

(define-message fuse_read_in
  ([user       _racket]
   [offset     _uint64]
   [size       _uint32]
   [rflags _read-flags]
   [lckown     _uint64]
   [flags       _oflag]
   [padding    _uint32]))

(define-message fuse_write_in
  ([user          _racket]
   [offset        _uint64]
   [size          _uint32]
   [flags          _oflag]
   [lckowner      _uint64]
   [wflags   _write-flags]
   [padding       _uint32]))

(define-message fuse_write_out
  ([size  _uint32]
   [dummy _uint32]))

(define-message fuse_statfs_out
  ([blocks  _uint64]
   [bfree   _uint64]
   [bavail  _uint64]
   [files   _uint64]
   [ffree   _uint64]
   [bsize   _uint32]
   [namelen _uint32]
   [frsize  _uint32]
   [padding _uint32]
   [spare1  _uint32]
   [spare2  _uint32]
   [spare3  _uint32]
   [spare4  _uint32]
   [spare5  _uint32]
   [spare6  _uint32]))

(define-message fuse_fsync_in
  ([user       _racket]
   [flags _fsync-flags]
   [pad        _uint32]))

(define-message fuse_setxattr_in
  ([size       _uint32]
   [op       _xattr-op]))

(define-message fuse_getxattr_in
  ([size _uint32]
   [pad  _uint32]))

(define-message fuse_getxattr_out
  ([size _uint32]
   [pad  _uint32]))

(define-message fuse_lk_in
  ([user      _racket]
   [owner     _uint64]
   [start     _uint64]
   [end       _uint64]
   [type      _uint32]
   [pid       _uint32]
   [flags _lock-flags]
   [pad       _uint32]))

(define-message fusk_lk_out
  ([start     _uint64]
   [end       _uint64]
   [type      _uint32]
   [pid       _uint32]))

(define-message fuse_access_in
  ([mask   _mode]
   [pad  _uint32]))

(define-message fuse_init_in
  ([major         _uint32]
   [minor         _uint32]
   [max_readahead _uint32]
   [flags     _fuse-flags]))

(define _unused_array (_array _uint32 9))

(define-message fuse_init_out_old
  ([major                _uint32]
   [minor                _uint32]
   [max_readahead        _uint32]
   [flags            _fuse-flags]
   [max_background       _uint16]
   [congestion_threshold _uint16]
   [max_write            _uint32]))

(define-message fuse_init_out
  ([major                _uint32]
   [minor                _uint32]
   [max_readahead        _uint32]
   [flags            _fuse-flags]
   [max_background       _uint16]
   [congestion_threshold _uint16]
   [max_write            _uint32]
   [time_gran            _uint32]
   [unused         _unused_array]))

(define-message fuse_interrupt_in
  ([unique _uint64]))

(define-message fuse_bmap_in
  ([block     _uint64]
   [blocksize _uint32]
   [padding   _uint32]))

(define-message fuse_bmap_out
  ([block _uint64]))

(define-message fuse_ioctl_in
  ([user       _racket]
   [flags _ioctl-flags]
   [cmd        _uint32]
   [arg        _uint64]
   [in_size    _uint32]
   [out_size   _uint32]))

(define-message fuse_ioctl_iovec
  ([base _uint64]
   [len  _uint64]))

(define-message fuse_ioctl_out
  ([result      _int32]
   [flags _ioctl-flags]
   [in_iovs    _uint32]
   [out_iovs   _uint32]))

(define-message fuse_poll_in
  ([user      _racket]
   [kh        _uint64]
   [flags _poll-flags]
   [events    _uint32]))

(define-message fuse_poll_out
  ([revents _uint32]
   [padding _uint32]))

(define-message fuse_notify_poll_wakeup_out
  ([kh _uint64]))

(define-message fuse_fallocate_in
  ([user    _racket]
   [offset  _uint64]
   [length  _uint64]
   [mode      _mode]
   [padding _uint32]))

(define-message fuse_in_header
  ([len     _uint32]
   [opcode  _opcode]
   [unique  _uint64]
   [nodeid  _uint64]
   [uid     _uint32]
   [gid     _uint32]
   [pid     _uint32]
   [padding _uint32]))

(define-message fuse_out_header
  ([len    _uint32]
   [error   _int32]
   [unique _uint64]))

(define-message fuse_dirent
  ([ino     _uint64]
   [offset  _uint64]
   [namelen _uint32]
   [type    _mode]))

(define-message fuse_notify_inval_inode_out
  ([inode  _uint64]
   [offset  _int64]
   [length _uint64]))

(define-message fuse_notify_inval_entry_out
  ([parent  _uint64]
   [namelen _uint32]
   [padding _uint32]))

(define-message fuse_notify_delete_out
  ([parent  _uint64]
   [child   _uint64]
   [namelen _uint32]
   [padding _uint32]))

(define-message fuse_notify_store_out
  ([nodeid  _uint64]
   [offset  _uint64]
   [size    _uint32]
   [padding _uint32]))

(define-message fuse_notify_retrieve_out
  ([notify_unique _uint64]
   [nodeid        _uint64]
   [offset        _uint64]
   [size          _uint32]
   [padding       _uint32]))

(define-message fuse_notify_retrieve_in
  ([dummy1 _uint64]
   [offset _uint64]
   [size   _uint32]
   [dummy2 _uint32]
   [dummy3 _uint64]
   [dummy4 _uint64]))
