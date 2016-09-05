#lang racket/base

(module+ test
  (require rackunit))

(require racket/contract
         racket/match
         (rename-in ffi/unsafe
                    [-> _->]))

(require "private/errno.rkt"
         "private/data.rkt"
         "private/filesystem.rkt")

(provide
 make-filesystem
 (contract-out [mount-filesystem (-> filesystem? path? (listof string?) void)]))

(define-logger fuse)

(struct session (filesystem channel [initialized #:mutable] [destroyed #:mutable]))

(define (mount-filesystem filesystem mountpoint options)
  (let ([session (make-session filesystem mountpoint options)])
    (run session)))

(define (make-session filesystem mountpoint options)
  (let ([channel (make-channel mountpoint options)])
    (unless (channel? channel)
      (let* ([errno   (code->errno channel)]
             [message (errno->message errno)])
        (log-fuse-error "Unable to mount filesystem: ~a: ~a" errno message)
        (raise-user-error 'fuse "Unable to mount filesystem: ~a: ~a" errno message)))
    (session filesystem channel #f #f)))

(define (run session)
  (match (receive (session-channel session))
    ['ENOENT (run session)] ; interrupted, retry...
    ['EINTR  (run session)] ; interrupted, retry...
    ['EAGAIN (run session)] ; try again...
    ['ENODEV (void)] ; filesystem was unmounted, quit
    [(? fuse-header? hdr)
     (begin
       (dispatch hdr session)
       (run session))]
    [errno
     (begin
       (log-fuse-error "Error communicating with FUSE device: errno = ~a" errno)
       (raise-user-error 'fuse "Error communicating with FUSE device: errno = ~a" errno))]))

(define MAX_WRITE_SIZE (* 16 1024 1024))
(define FUSE_MAJOR 7)
(define FUSE_MINOR 22)

(define (dispatch hdr session)
  (define (reply-error errno)
    (send (session-channel session) (fuse-header-unique hdr) errno #f))
  (define (reply-ok data [ctype #f])
    (send (session-channel session) (fuse-header-unique hdr) #f data ctype))
  (define (reply-empty)
    (reply-ok #f))
  (define (reply-attr #:attr-valid valid #:inode inode #:rdev rdev
                      #:size size #:blocks blocks #:atime atime
                      #:mtime mtime #:ctime ctime #:kind kind
                      #:perm perm #:nlink nlink #:uid uid #:gid gid)
    (let ([data (make-fuse_attr_out (timespec-sec valid)
                                    (timespec-nsec valid)
                                    0
                                    inode
                                    size
                                    blocks
                                    (timespec-sec atime)
                                    (timespec-sec mtime)
                                    (timespec-sec ctime)
                                    (timespec-nsec atime)
                                    (timespec-nsec mtime)
                                    (timespec-nsec ctime)
                                    (or-flags kind perm)
                                    nlink
                                    uid
                                    gid
                                    rdev
                                    0
                                    0)])
      (send (session-channel session) (fuse-header-unique hdr) #f data _fuse_attr_out)))
  (define (reply-entry #:entry-valid ent-valid #:attr-valid attr-valid
                       #:generation generation #:inode inode #:rdev rdev
                       #:size size #:blocks blocks #:atime atime
                       #:mtime mtime #:ctime ctime #:kind kind
                       #:perm perm #:nlink nlink #:uid uid #:gid gid)
    (let ([data (make-fuse_entry_out inode
                                     generation
                                     (timespec-sec ent-valid)
                                     (timespec-sec attr-valid)
                                     (timespec-nsec ent-valid)
                                     (timespec-nsec attr-valid)
                                     inode
                                     size
                                     blocks
                                     (timespec-sec atime)
                                     (timespec-sec mtime)
                                     (timespec-sec ctime)
                                     (timespec-nsec atime)
                                     (timespec-nsec mtime)
                                     (timespec-nsec ctime)
                                     (or-flags kind perm)
                                     nlink
                                     uid
                                     gid
                                     rdev
                                     0
                                     0)])
      (send (session-channel session) (fuse-header-unique hdr) #f data _fuse_entry_out)))
  (define (reply-open #:info info #:flags flags)
    (let ([data (make-fuse_open_out info flags 0)])
      (send (session-channel session) (fuse-header-unique hdr) #f data _fuse_open_out)))
  (log-fuse-info "handling ~a" (fuse-header-opcode hdr))
  (match (fuse-header-opcode hdr)
    ['FUSE_INIT
     (let* ([init  (filesystem-init (session-filesystem session))]
            [in    (decode-payload hdr _fuse_init_in)]
            [major (fuse_init_in-major in)]
            [minor (fuse_init_in-minor in)]
            [max_readahead (fuse_init_in-max_readahead in)]
            [flags (ptr-ref (ptr-add in fuse_init_in-max_readahead-offset) _fuse_flags)]
            [legacy-init (and (>= major 7) (< minor 23))])
       (when (or (< major 7) (and (= major 7) (< minor 6)))
         (reply-error 'EPROTO)
         (log-fuse-error "Unsupported FUSE ABI version ~a.~a" major minor)
         (raise-user-error "Unsupported FUSE ABI version ~a.~a" major minor))
       (log-fuse-info "FUSE ABI version ~a.~a" major minor)
       (match (init)
         [#t
          (let ([init_out (if legacy-init
                              (make-fuse_init_out_old
                               FUSE_MAJOR
                               FUSE_MINOR
                               max_readahead
                               (filter (lambda (f) (member f '(FUSE_ASYNC_READ FUSE_EXPORT_SUPPORT FUSE_BIG_WRITES))) flags)
                               0
                               0
                               MAX_WRITE_SIZE)
                              (make-fuse_init_out
                               FUSE_MAJOR
                               FUSE_MINOR
                               max_readahead
                               (filter (lambda (f) (member f '(FUSE_ASYNC_READ FUSE_EXPORT_SUPPORT FUSE_BIG_WRITES))) flags)
                               0
                               0
                               MAX_WRITE_SIZE
                               0
                               unused-init-array))])
            (set-session-initialized! session #t)
            (reply-ok init_out (if legacy-init _fuse_init_out_old _fuse_init_out)))]
         [errno
          (reply-error errno)]))]
    [(? (lambda (op) (not (session-initialized session))) op)
     (reply-error 'EIO)]
    ['FUSE_DESTROY
     (let* ([destroy (filesystem-destroy (session-filesystem session))])
       (destroy)
       (set-session-destroyed! session #t)
       (reply-empty))]
    [(? (lambda (op) (session-destroyed session)) op)
     (reply-error 'EIO)]
    ['FUSE_FORGET
     (let* ([forget  (filesystem-forget (session-filesystem session))]
            [nodeid  (fuse-header-nodeid hdr)]
            [nlookup (ptr-ref (fuse-header-payload hdr) _uint64)])
       (forget #:nodeid nodeid #:nlookup nlookup))]
    ['FUSE_GETATTR
     (let* ([getattr (filesystem-getattr (session-filesystem session))]
            [nodeid  (fuse-header-nodeid hdr)]
            [info    #f])
       (getattr #:nodeid nodeid #:info info #:reply reply-attr #:error reply-error))]
    ['FUSE_OPENDIR
     (let* ([opendir (filesystem-opendir (session-filesystem session))]
            [nodeid  (fuse-header-nodeid hdr)]
            [flags   (ptr-ref (fuse-header-payload hdr) _flag_t)])
       (opendir #:nodeid nodeid #:flags flags #:reply reply-open #:error reply-error))]
    ['FUSE_READDIR
     (let* ([readdir (filesystem-readdir (session-filesystem session))]
            [nodeid  (fuse-header-nodeid hdr)]
            [in      (decode-payload hdr _fuse_read_in)]
            [info    (ptr-ref in _racket)]
            [offset  (fuse_read_in-offset in)]
            [size    (fuse_read_in-size   in)]
            [buffer  (malloc 'atomic size)]
            [used    0]
            [add     (lambda (#:inode inode #:offset offset #:kind kind #:name name)
                       (let* ([name    (path->bytes name)]
                              [namelen (bytes-length name)]
                              [entlen  (+ (ctype-sizeof _fuse_dirent) namelen)]
                              [rem     (remainder entlen (ctype-sizeof _uint64))]
                              [entsize (if (= 0 rem) entlen (+ (- entlen rem) (ctype-sizeof _uint64)))]
                              [padlen  (- entsize entlen)])
                         (if (> (+ used entsize) size)
                             #f
                             (let* ([header  (ptr-add buffer used)]
                                    [data    (ptr-add header (ctype-sizeof _fuse_dirent))]
                                    [padding (ptr-add data namelen)])
                               (ptr-set! header _fuse_dirent (make-fuse_dirent inode offset namelen kind))
                               (memcpy data name namelen)
                               (memset padding 0 padlen)
                               (set! used (+ used entsize))
                               #t))))]
            [done    (lambda ()
                       (send (session-channel session) (fuse-header-unique hdr) #f buffer used))])
       (readdir #:nodeid nodeid #:info info #:offset offset #:add add #:reply done #:error reply-error))]
    ['FUSE_RELEASEDIR
     (let* ([releasedir (filesystem-releasedir (session-filesystem session))]
            [nodeid     (fuse-header-nodeid hdr)]
            [in         (decode-payload hdr _fuse_release_in)]
            [info       (ptr-ref in _racket)]
            [flags      (ptr-ref (ptr-add in fuse_release_in-flags-offset) _fuse_flags)])
       (releasedir #:nodeid nodeid #:info info #:flags flags #:reply reply-empty #:error reply-error))]
    ['FUSE_LOOKUP
     (let* ([lookup     (filesystem-lookup (session-filesystem session))]
            [nodeid     (fuse-header-nodeid hdr)]
            [name       (bytes->path-element (cast (fuse-header-payload hdr) _pointer _bytes))])
       (log-fuse-info "lookup ~a -> ~a" nodeid name)
       (lookup #:nodeid nodeid #:name name #:reply reply-entry #:error reply-error))]
    ['FUSE_OPEN
     (let* ([open       (filesystem-open (session-filesystem session))]
            [nodeid     (fuse-header-nodeid hdr)]
            [flags      (decode-payload hdr _flag_t)])
       (open #:nodeid nodeid #:flags flags #:reply reply-open #:error reply-error))]
    ['FUSE_READ
     (let* ([read       (filesystem-read (session-filesystem session))]
            [nodeid     (fuse-header-nodeid hdr)]
            [in         (decode-payload hdr _fuse_read_in)]
            [info       (ptr-ref in _racket)]
            [offset     (fuse_read_in-offset in)]
            [size       (fuse_read_in-size   in)])
       (read #:nodeid nodeid #:info info #:offset offset #:size size #:reply reply-ok #:error reply-error))]
    [op (reply-error 'ENOSYS)]
    ['FUSE_SETATTR '()]
    ['FUSE_READLINK '()]
    ['FUSE_SYMLINK '()]
    ['FUSE_MKNOD '()]
    ['FUSE_MKDIR '()]
    ['FUSE_UNLINK '()]
    ['FUSE_RMDIR '()]
    ['FUSE_RENAME '()]
    ['FUSE_LINK '()]
    ['FUSE_WRITE '()]
    ['FUSE_STATFS '()]
    ['FUSE_RELEASE '()]
    ['FUSE_FSYNC '()]
    ['FUSE_SETXATTR '()]
    ['FUSE_GETXATTR '()]
    ['FUSE_LISTXATTR '()]
    ['FUSE_REMOVEXATTR '()]
    ['FUSE_FLUSH '()]
    ['FUSE_FSYNCDIR '()]
    ['FUSE_GETLK '()]
    ['FUSE_SETLK '()]
    ['FUSE_SETLKW '()]
    ['FUSE_ACCESS '()]
    ['FUSE_CREATE '()]
    ['FUSE_INTERRUPT '()]
    ['FUSE_BMAP '()]))

(define libfuse (ffi-lib "libfuse"))
(define libc (ffi-lib #f))
(define libracket (ffi-lib #f))

(define-cstruct _fuse_args
  ([argc      _int]
   [argv      (_list i _string)]
   [allocated _int]))

(define fuse_mount_compat25
  (get-ffi-obj "fuse_mount_compat25" libfuse
               (_fun #:save-errno 'posix _path _fuse_args-pointer _-> _int)))

(define read
  (get-ffi-obj "read" libc
               (_fun #:save-errno 'posix _int _gcpointer _size _-> _int)))

(define write
  (get-ffi-obj "write" libc
               (_fun #:save-errno 'posix _int _gcpointer _size _-> _int)))

(struct channel (fd path))

(define (make-channel mountpoint options)
  (let* ([real-path (resolve-path mountpoint)]
         [args (make-fuse_args (length options) options 0)]
         [ret (fuse_mount_compat25 real-path args)])
    (if (eq? ret -1)
        (saved-errno)
        (channel ret real-path))))

(define BUFSIZE (+ (* 16 1024 1024) 4096))

(define (receive channel)
  (let* ([buffer (malloc 'atomic BUFSIZE)]
         [ret (read (channel-fd channel) buffer BUFSIZE)])
    (if (eq? ret -1)
        (match (saved-errno)
          [2 'ENOENT]
          [4  'EINTR]
          [11 'EAGAIN]
          [19 'ENODEV]
          [errno  errno])
        (let ([hdr (ptr-ref buffer _fuse_header)])
          (set-fuse-header-payload! hdr (ptr-add buffer 1 _fuse_in_header))
          hdr))))

(define (send channel unique errno data [ctype-or-size #f])
  (define data-size
    (cond
      [(not data) 0]
      [(not ctype-or-size) (bytes-length data)]
      [(ctype? ctype-or-size) (ctype-sizeof ctype-or-size)]
      [else ctype-or-size]))
  (define size (+ (ctype-sizeof _fuse_out_header) data-size))
  (define buffer (malloc 'atomic size))
  (define code (if errno (- (errno->code errno)) 0))
  (ptr-set! buffer _fuse_out_header (make-fuse_out_header size code unique))
  (when data (memcpy buffer (ctype-sizeof _fuse_out_header) data data-size))
  (let ([written (write (channel-fd channel) buffer size)]) ; buffer size
    (unless (eq? size written)
      (let* ([code (saved-errno)]
             [errno (code->errno code)])
        (log-fuse-warning "send failed: ~a: ~a" errno (errno->message errno))
        (when (eq? errno 'EINVAL)
          (raise-user-error 'fuse "send: Invalid argument"))))))

(define-cstruct _fuse_out_header
  ([len    _uint32]
   [error  _int32]
   [unique _uint64]))

(define _opcode
  (_enum '(FUSE_LOOKUP = 1
           FUSE_FORGET = 2
           FUSE_GETATTR = 3
           FUSE_SETATTR = 4
           FUSE_READLINK = 5
           FUSE_SYMLINK = 6
           FUSE_MKNOD = 8
           FUSE_MKDIR = 9
           FUSE_UNLINK = 10
           FUSE_RMDIR = 11
           FUSE_RENAME = 12
           FUSE_LINK = 13
           FUSE_OPEN = 14
           FUSE_READ = 15
           FUSE_WRITE = 16
           FUSE_STATFS = 17
           FUSE_RELEASE = 18
           FUSE_FSYNC = 20
           FUSE_SETXATTR = 21
           FUSE_GETXATTR = 22
           FUSE_LISTXATTR = 23
           FUSE_REMOVEXATTR = 24
           FUSE_FLUSH = 25
           FUSE_INIT = 26
           FUSE_OPENDIR = 27
           FUSE_READDIR = 28
           FUSE_RELEASEDIR = 29
           FUSE_FSYNCDIR = 30
           FUSE_GETLK = 31
           FUSE_SETLK = 32
           FUSE_SETLKW = 33
           FUSE_ACCESS = 34
           FUSE_CREATE = 35
           FUSE_INTERRUPT = 36
           FUSE_BMAP = 37
           FUSE_DESTROY = 38
           FUSE_IOCTL = 39
           FUSE_POLL = 40
           FUSE_NOTIFY_REPLY = 41
           FUSE_BATCH_FORGET = 42
           FUSE_FALLOCATE = 43
           FUSE_READDIRPLUS = 44
           FUSE_RENAME2 =45)
         _uint32))

(define-cstruct _fuse_in_header
  ([len     _uint32]
   [opcode  _opcode]
   [unique  _uint64]
   [nodeid  _uint64]
   [uid     _uint32]
   [gid     _uint32]
   [pid     _uint32]
   [padding _uint32]))

(struct fuse-header (len opcode unique nodeid uid gid pid [payload #:mutable]))

(define _fuse_header
  (make-ctype
   _fuse_in_header
   (lambda (hdr)
     (make-fuse_in_header
      (fuse-header-len hdr)
      (fuse-header-opcode hdr)
      (fuse-header-unique hdr)
      (fuse-header-nodeid hdr)
      (fuse-header-uid hdr)
      (fuse-header-gid hdr)
      (fuse-header-pid hdr)
      0))
   (lambda (hdr)
     (fuse-header
      (fuse_in_header-len hdr)
      (fuse_in_header-opcode hdr)
      (fuse_in_header-unique hdr)
      (fuse_in_header-nodeid hdr)
      (fuse_in_header-uid hdr)
      (fuse_in_header-gid hdr)
      (fuse_in_header-pid hdr)
      #f))))

(define _setattr_valid
  (_bitmask '(FATTR_MODE  = 1
              FATTR_UID   = 2
              FATTR_GID   = 4
              FATTR_SIZE  = 8
              FATTR_ATIME = 16
              FATTR_MTIME = 32
              FATTR_USER  = 64)
            _uint32))

(define-cstruct _fuse_open_out
  ([user    _racket]
   [flags   _flag_t]
   [padding _uint32]))

(define-cstruct _fuse_read_in
  ([user    _racket]
   [offset  _uint64]
   [size    _uint32]
   [rflags  _uint32]
   [lckown  _uint64]
   [flags   _flag_t]
   [padding _uint32])
  #:define-unsafe)

(define-cstruct _fuse_release_in
  ([user          _racket]
   [flags         _flag_t]
   [release_flags _uint32]
   [lock_owner    _uint64])
  #:define-unsafe)

(define-cstruct _fuse_dirent
  ([ino     _uint64]
   [offset  _uint64]
   [namelen _uint32]
   [type    _mode_t]))

(define-cstruct _fuse_in_setattr
  ([valid _setattr_valid]
   [padding _uint32]
   [user _racket]
   [size _uint64]
   [unused1 _uint64]
   [atime _int64]
   [mtime _int64]
   [unused2 _uint64]
   [atimensec _int32]
   [mtimensec _int32]
   [unused3 _uint32]
   [mode _uint32]
   [unused4 _uint32]
   [uid _uint32]
   [gid _uint32]
   [unused5 _uint32]))

(define-cstruct _fuse_attr_out
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
   [mode            _mode_t]
   [nlink           _uint32]
   [uid             _uint32]
   [gid             _uint32]
   [rdev            _uint32]
   [blksize         _uint32]
   [padding         _uint32]))

(define-cstruct _fuse_entry_out
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
   [mode            _mode_t]
   [nlink           _uint32]
   [uid             _uint32]
   [gid             _uint32]
   [rdev            _uint32]
   [blksize         _uint32]
   [padding         _uint32]))

(define _fuse_flags
  (_bitmask '(FUSE_ASYNC_READ     = 1
              FUSE_POSIX_LOCKS    = 2
              FUSE_FILE_OPS       = 4
              FUSE_ATOMIC_O_TRUNC = 8
              FUSE_EXPORT_SUPPORT = 16
              FUSE_BIG_WRITES     = 32
              FUSE_DONT_MASK      = 64)
            _uint32))

(define-cstruct _fuse_init_in
  ([major _uint32]
   [minor _uint32]
   [max_readahead _uint32]
   [flags _fuse_flags])
  #:define-unsafe)

(define _unused_array (_array _uint32 9))
(define unused-init-array
  (let ([buffer (malloc 'atomic _unused_array)])
    (ptr-ref buffer _unused_array)))

(define-cstruct _fuse_init_out_old
  ([major _uint32]
   [minor _uint32]
   [max_readahead _uint32]
   [flags _fuse_flags]
   [max_background _uint16]
   [congestion_threshold _uint16]
   [max_write _uint32]))

(define-cstruct _fuse_init_out
  ([major _uint32]
   [minor _uint32]
   [max_readahead _uint32]
   [flags _fuse_flags]
   [max_background _uint16]
   [congestion_threshold _uint16]
   [max_write _uint32]
   [time_gran _uint32]
   [unused _unused_array]))

(define (decode-payload hdr type)
  (ptr-ref (fuse-header-payload hdr) type))

(module+ test
  (let* ([lookup (lambda (#:nodeid nodeid #:name name #:reply reply-entry #:error error)
                   (if (and (= nodeid 1) (equal? name (string->path "hello.txt")))
                       (reply-entry #:generation 0 #:entry-valid (timespec 1 0) #:attr-valid (timespec 1 0)
                                    #:inode 2 #:rdev 0 #:size 13 #:blocks 1
                                    #:atime (timespec 1381237736 0) #:mtime (timespec 1381237736 0)
                                    #:ctime (timespec 1381237736 0) #:kind 'S_IFREG
                                    #:perm '(S_IRUSR S_IWUSR S_IRGRP S_IROTH)
                                    #:nlink 1 #:uid 1000 #:gid 1000)
                       (error 'ENOENT)))]
         [getattr (lambda (#:nodeid nodeid #:info info #:reply reply-attr #:error error)
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
                      [_ (error 'ENOENT)]))]
         [readdir (lambda (#:nodeid nodeid #:info info #:offset offset #:add reply-add #:reply reply-done #:error error)
                    (if (= nodeid 1)
                        (begin
                          (when (= offset 0)
                            (reply-add #:inode 1 #:offset 0 #:kind 'S_IFDIR #:name (string->path "."))
                            (reply-add #:inode 1 #:offset 1 #:kind 'S_IFDIR #:name (string->path ".."))
                            (reply-add #:inode 1 #:offset 2 #:kind 'S_IFREG #:name (string->path "hello.txt")))
                          (reply-done))
                        (error 'ENOENT)))]
         [open (lambda (#:nodeid nodeid #:flags flags #:reply reply-open #:error error)
                 (log-fuse-debug "open ~a ~a" nodeid flags)
                 (if (= nodeid 2)
                     (reply-open #:info #f #:flags flags)
                     (error 'EISDIR)))]
         [read (lambda (#:nodeid nodeid #:info info #:offset offset #:size size #:reply reply-data #:error error)
                 (reply-data (string->bytes/utf-8 "Hello world!\n")))]
         [hello (make-filesystem #:lookup lookup #:getattr getattr #:readdir readdir #:open open #:read read)]
         [session (make-session hello (string->path "/home/vagrant/tmp") (list "default_permissions" "allow_other" "large_read" "direct_io" "hard_remove"))])
    (run session)))
