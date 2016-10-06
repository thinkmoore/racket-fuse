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
 (struct-out timespec)
 (contract-out [mount-filesystem (-> filesystem? path? (listof string?) void)])
 errno? modes/c oflags/c
 reply-entry/c reply-empty/c reply-data/c reply-attr/c reply-error/c reply-write/c reply-create/c)

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
    [(? cpointer? buffer)
     (let ([header  (decode-fuse_in_header buffer)]
           [payload (skip-fuse_in_header buffer)])
       (dispatch header payload session)
       (run session))]
    [errno
     (begin
       (log-fuse-error "Error communicating with FUSE device: errno = ~a" errno)
       (raise-user-error 'fuse "Error communicating with FUSE device: errno = ~a" errno))]))

(define MAX_WRITE_SIZE (* 16 1024 1024))
(define FUSE_MAJOR 7)
(define FUSE_MINOR 22)

(define (dispatch header payload session)
  (define unique (fuse_in_header-unique header))
  (define nodeid (fuse_in_header-nodeid header))
  (define (reply-error errno)
    (send (session-channel session) unique errno #f))
  (define (reply-ok data [ctype #f])
    (send (session-channel session) unique #f data ctype))
  (define (make-reply-sized size)
    (lambda (data)
      (if (>= (bytes-length data) size)
          (send (session-channel session) unique 'ERANGE #f)
          (send (session-channel session) unique #f data))))
  (define (make-reply-sized-list size)
    (lambda (data)
      (let ([buffer (malloc 'atomic size)])
        (memset buffer 0 size)
        (define-values (avail next)
          (for/fold
              ([avail size]
               [next  buffer])
              ([value (in-list data)]
               #:break (not avail))
            (let ([len (bytes-length value)])
              (if (>= len avail)
                  (begin
                    (send (session-channel session) unique 'ERANGE #f)
                    (values #f next))
                  (begin
                    (memcpy next value len)
                    (values (- avail (add1 len))))))))
        (when avail
          (send (session-channel session) unique #f buffer (- size avail))))))
  (define (reply-empty)
    (reply-ok #f))
  (define (reply-attr #:attr-valid valid #:inode inode #:rdev rdev
                      #:size size #:blocks blocks #:atime atime
                      #:mtime mtime #:ctime ctime #:kind kind
                      #:perm perm #:nlink nlink #:uid uid #:gid gid)
    (let ([data (fuse_attr_out (timespec-sec valid)
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
      (send (session-channel session) unique #f data _fuse_attr_out)))
  (define (reply-entry #:entry-valid ent-valid #:attr-valid attr-valid
                       #:generation generation #:inode inode #:rdev rdev
                       #:size size #:blocks blocks #:atime atime
                       #:mtime mtime #:ctime ctime #:kind kind
                       #:perm perm #:nlink nlink #:uid uid #:gid gid)
    (let ([data (fuse_entry_out inode
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
      (send (session-channel session) unique #f data _fuse_entry_out)))
  (define (reply-open #:info info #:flags flags)
    (let ([data (fuse_open_out info flags 0)])
      (send (session-channel session) unique #f data _fuse_open_out)))
  (define (reply-write #:written written)
    (let ([data (fuse_write_out written 0)])
      (send (session-channel session) unique #f data _fuse_write_out)))
  (define (reply-lock #:start start #:end end #:type type #:pid pid)
    (let ([data (fuse_lk_out start end type pid)])
      (send (session-channel session) unique #f data _fuse_lk_out)))
  (define (reply-bmap #:index index)
    (send (session-channel session) unique #f index _uint64))
  (define (reply-create #:entry-valid ent-valid #:attr-valid attr-valid
                        #:generation generation #:inode inode #:rdev rdev
                        #:size size #:blocks blocks #:atime atime
                        #:mtime mtime #:ctime ctime #:kind kind
                        #:perm perm #:nlink nlink #:uid uid #:gid gid
                        #:info info #:flags flags)
    (let ([size   (+ (ctype-sizeof _fuse_entry_out) (ctype-sizeof _fuse_open_out))]
          [buffer (malloc 'atomic size)]
          [entry  (fuse_entry_out inode
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
                                  0)]
          [open   (fuse_open_out info flags 0)])
      (ptr-set! buffer _fuse_entry_out entry)
      (ptr-set! buffer _fuse_open_out open)
      (send (session-channel session) unique #f buffer size)))
  (define (reply-statfs #:blocks blocks #:bfree bfree #:bavail bavail #:files files
                        #:ffree ffree #:bsize bsize #:namelen namelen #:frsize frsize)
    (let ([data (fuse_statfs_out blocks
                                 bfree
                                 bavail
                                 files
                                 ffree
                                 bsize
                                 namelen
                                 frsize)])
      (send (session-channel session) unique #f data _fuse_statfs_out)))
  (parameterize ([request-pid (fuse_in_header-pid header)]
                 [request-uid (fuse_in_header-uid header)]
                 [request-gid (fuse_in_header-gid header)])
    (match (fuse_in_header-opcode header)
      ['FUSE_INIT
       (let* ([init  (filesystem-init (session-filesystem session))]
              [in    (decode-fuse_init_in payload)]
              [major (fuse_init_in-major in)]
              [minor (fuse_init_in-minor in)]
              [max_readahead (fuse_init_in-max_readahead in)]
              [flags (fuse_init_in-flags in)]
              [legacy-init (and (>= major 7) (< minor 23))])
         (log-fuse-debug "FUSE_INIT major: ~a minor: ~a flags: ~a" major minor flags)
         (when (or (< major 7) (and (= major 7) (< minor 6)))
           (reply-error 'EPROTO)
           (log-fuse-error "Unsupported FUSE ABI version ~a.~a" major minor)
           (raise-user-error "Unsupported FUSE ABI version ~a.~a" major minor))
         (log-fuse-info "FUSE ABI version ~a.~a" major minor)
         (match (init)
           [#t
            (let ([init_out (if legacy-init
                                (fuse_init_out_old
                                 FUSE_MAJOR
                                 FUSE_MINOR
                                 max_readahead
                                 (filter (lambda (f) (member f '(FUSE_ASYNC_READ FUSE_EXPORT_SUPPORT FUSE_BIG_WRITES))) flags)
                                 0
                                 0
                                 MAX_WRITE_SIZE)
                                (fuse_init_out
                                 FUSE_MAJOR
                                 FUSE_MINOR
                                 max_readahead
                                 (filter (lambda (f) (member f '(FUSE_ASYNC_READ FUSE_EXPORT_SUPPORT FUSE_BIG_WRITES))) flags)
                                 0
                                 0
                                 MAX_WRITE_SIZE
                                 0
                                 (let* ([_unused_array (_array _uint32 9)]
                                        [buffer (malloc 'atomic _unused_array)])
                                   (ptr-ref buffer _unused_array))))])
              (set-session-initialized! session #t)
              (reply-ok init_out (if legacy-init _fuse_init_out_old _fuse_init_out)))]
           [errno
            (reply-error errno)]))]
      [(? (lambda (op) (not (session-initialized session))) op)
       (log-fuse-error "Operation ~a received for uninitialized session" op)
       (reply-error 'EIO)]
      ['FUSE_DESTROY
       (let* ([destroy (filesystem-destroy (session-filesystem session))])
         (log-fuse-info "FUSE_DESTROY")
         (destroy)
         (set-session-destroyed! session #t)
         (reply-empty))]
      [(? (lambda (op) (session-destroyed session)) op)
       (log-fuse-error "Operation ~a received for destroyed session" op)
       (reply-error 'EIO)]
      ['FUSE_FORGET
       (let* ([forget  (filesystem-forget (session-filesystem session))]
              [in      (decode-fuse_forget_in payload)]
              [nlookup (fuse_forget_in-nlookup in)])
         (log-fuse-info "FUSE_FORGET nodeid: ~a nlookup: ~a" nodeid nlookup)
         (forget #:nodeid nodeid #:nlookup nlookup))]
      ['FUSE_GETATTR
       (let* ([getattr (filesystem-getattr (session-filesystem session))]
              [in      (decode-fuse_getattr_in payload)]
              [flags   (fuse_getattr_in-flags in)]
              [info    (if (eq? flags 'FUSE_GETATTR_FH) (fuse_getattr_in-user in) #f)])
         (log-fuse-info "FUSE_GETATTR nodeid: ~a flags: ~a" nodeid flags)
         (getattr #:nodeid nodeid #:info info #:reply reply-attr #:error reply-error))]
      ['FUSE_OPENDIR
       (let* ([opendir (filesystem-opendir (session-filesystem session))]
              [in      (decode-fuse_open_in payload)]
              [flags   (fuse_open_in-flags in)])
         (log-fuse-info "FUSE_OPENDIR nodeid: ~a flags: ~a" nodeid flags)
         (opendir #:nodeid nodeid #:flags flags #:reply reply-open #:error reply-error))]
      ['FUSE_READDIR
       (let* ([readdir (filesystem-readdir (session-filesystem session))]
              [in      (decode-fuse_read_in payload)]
              [info    (fuse_read_in-user in)]
              [offset  (fuse_read_in-offset in)]
              [size    (fuse_read_in-size in)]
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
                                 (ptr-set! header _fuse_dirent 0 (fuse_dirent inode offset namelen kind))
                                 (memcpy data name namelen)
                                 (memset padding 0 padlen)
                                 (set! used (+ used entsize))
                                 #t))))]
              [done    (lambda ()
                         (send (session-channel session) unique #f buffer used))])
         (log-fuse-info "FUSE_READDIR nodeid: ~a offset: ~a" nodeid offset)
         (readdir #:nodeid nodeid #:info info #:offset offset #:add add #:reply done #:error reply-error))]
      ['FUSE_RELEASEDIR
       (let* ([releasedir    (filesystem-releasedir (session-filesystem session))]
              [in            (decode-fuse_release_in payload)]
              [info          (fuse_release_in-user in)]
              [flags         (fuse_release_in-flags in)]
              [release-flags (fuse_release_in-rflags in)]
              [lockowner     (fuse_release_in-lckowner in)])
         (log-fuse-info "FUSE_RELEASEDIR nodeid: ~a flags: ~a" nodeid flags)
         (releasedir #:nodeid nodeid #:info info #:flags flags #:reply reply-empty #:error reply-error))]
      ['FUSE_LOOKUP
       (let* ([lookup     (filesystem-lookup (session-filesystem session))]
              [name       (bytes->path-element (cast payload _pointer _bytes))])
         (log-fuse-info "FUSE_LOOKUP nodeid: ~a name: ~a" nodeid name)
         (lookup #:nodeid nodeid #:name name #:reply reply-entry #:error reply-error))]
      ['FUSE_OPEN
       (let* ([open       (filesystem-open (session-filesystem session))]
              [in         (decode-fuse_open_in payload)]
              [flags      (fuse_open_in-flags in)])
         (log-fuse-info "FUSE_OPEN nodeid: ~a flags: ~a" nodeid flags)
         (open #:nodeid nodeid #:flags flags #:reply reply-open #:error reply-error))]
      ['FUSE_READ
       (let* ([read       (filesystem-read (session-filesystem session))]
              [in         (decode-fuse_read_in payload)]
              [info       (fuse_read_in-user in)]
              [offset     (fuse_read_in-offset in)]
              [size       (fuse_read_in-size in)]
              [flags      (fuse_read_in-flags in)]
              [lockowner  (and (check-flag (fuse_read_in-rflags in) (fuse_read_in-lckown in)))])
         (log-fuse-info "FUSE_READ nodeid: ~a offset: ~a size: ~a flags: ~a lockowner: ~a" nodeid offset size flags lockowner)
         (read #:nodeid nodeid #:info info #:offset offset #:size size #:lockowner lockowner
               #:reply reply-ok #:error reply-error))]
      ['FUSE_SETATTR ;XXX Should handle FATTR_FH and FATTR_LOCKOWNER
       (let* ([setattr    (filesystem-setattr (session-filesystem session))]
              [in         (decode-fuse_setattr_in payload)]
              [info       (fuse_setattr_in-user in)]
              [valid      (fuse_setattr_in-valid in)]
              [mode       (and (check-flag valid 'FATTR_MODE)
                               (fuse_setattr_in-mode in))]
              [uid        (and (check-flag valid 'FATTR_UID)
                               (fuse_setattr_in-uid in))]
              [gid        (and (check-flag valid 'FATTR_GID)
                               (fuse_setattr_in-gid in))]
              [size       (and (check-flag valid 'FATTR_SIZE)
                               (fuse_setattr_in-size in))]
              [atime      (or (and (check-flag valid 'FATTR_ATIME) (timespec (fuse_setattr_in-atime in) (fuse_setattr_in-atimensec in)))
                              (and (check-flag valid 'FATTR_ATIME_NOW) (timespec-now)))]
              [mtime      (or (and (check-flag valid 'FATTR_MTIME) (timespec (fuse_setattr_in-mtime in) (fuse_setattr_in-mtimensec in)))
                              (and (check-flag valid 'FATTR_MTIME_NOW) (timespec-now)))]
              [ctime      (and (check-flag valid 'FATTR_CTIME) (timespec (fuse_setattr_in-ctime in) (fuse_setattr_in-ctimensec in)))])
         (log-fuse-info "FUSE_SETATTR nodeid: ~a mode: ~a uid: ~a gid: ~a size: ~a atime: ~a mtime: ~a ctime: ~a"
                        nodeid mode uid gid size atime mtime ctime)
         (if (or (check-flag valid 'FATTR_FH) (check-flag valid 'FATTR_LOCKOWNER))
             (reply-error 'ENOSYS)
             (setattr #:nodeid nodeid #:info info #:mode mode #:uid uid #:gid gid #:size size
                      #:atime atime #:mtime mtime #:ctime ctime #:reply reply-attr #:error reply-error)))]
      ['FUSE_READLINK
       (let* ([readlink   (filesystem-readlink (session-filesystem session))])
         (log-fuse-info "FUSE_READLINK nodeid: ~a" nodeid)
         (readlink #:nodeid nodeid #:reply reply-ok #:error reply-error))]
      ['FUSE_SYMLINK
       (let* ([symlink    (filesystem-symlink (session-filesystem session))]
              [name-bytes (cast payload _pointer _bytes)]
              [name-len   (bytes-length name-bytes)]
              [name       (bytes->path-element name-bytes)]
              [link       (bytes->path-element (cast (ptr-add payload (+ name-len 1)) _pointer _bytes))])
         (log-fuse-info "FUSE_SYMLINK nodeid: ~a name: ~a link: ~a" nodeid name link)
         (symlink #:nodeid nodeid #:name name #:link link #:reply reply-entry #:error reply-error))]
      ['FUSE_MKNOD
       (let* ([mknod      (filesystem-mknod (session-filesystem session))]
              [in         (decode-fuse_mknod_in payload)]
              [name       (bytes->path-element (cast (skip-fuse_mknod_in payload) _pointer _bytes))]
              [modeinfo   (fuse_mknod_in-mode in)]
              [kind       (filter filetype? modeinfo)]
              [mode       (filter perm? modeinfo)]
              [umask      (fuse_mknod_in-umask in)]
              [rdev       (fuse_mknod_in-rdev in)])
         (log-fuse-info "FUSE_MKNOD nodeid: ~a name: ~a kind: ~a mode: ~a umask: ~a rdev: ~a" nodeid name kind mode umask rdev)
         (mknod #:nodeid nodeid #:name name #:kind kind #:mode mode #:umask umask #:rdev rdev
                #:reply reply-entry #:error reply-error))]
      ['FUSE_MKDIR
       (let* ([mkdir      (filesystem-mkdir (session-filesystem session))]
              [in         (decode-fuse_mkdir_in payload)]
              [mode       (fuse_mkdir_in-mode in)]
              [umask      (fuse_mkdir_in-umask in)]
              [name       (bytes->path-element (cast (skip-fuse_mkdir_in payload) _pointer _bytes))])
         (log-fuse-info "FUSE_MKDIR nodeid: ~a name: ~a mode: ~a umask: ~a" nodeid name mode umask)
         (mkdir #:nodeid nodeid #:name name #:mode mode #:umask umask #:reply reply-entry #:error reply-error))]
      ['FUSE_UNLINK
       (let* ([unlink     (filesystem-unlink (session-filesystem session))]
              [name       (bytes->path-element (cast payload _pointer _bytes))])
         (log-fuse-info "FUSE_UNLINK nodeid: ~a name: ~a" nodeid name)
         (unlink #:nodeid nodeid #:name name #:reply reply-empty #:error reply-error))]
      ['FUSE_RMDIR
       (let* ([rmdir      (filesystem-rmdir (session-filesystem session))]
              [name       (bytes->path-element (cast payload _pointer _bytes))])
         (log-fuse-info "FUSE_RMDIR nodeid: ~a name: ~a" nodeid name)
         (rmdir #:nodeid nodeid #:name name #:reply reply-empty #:error reply-error))]
      ['FUSE_RENAME
       (let* ([rename      (filesystem-rename (session-filesystem session))]
              [in          (decode-fuse_rename_in payload)]
              [newnodeid   (fuse_rename_in-newdir in)]
              [name-ptr    (skip-fuse_rename_in payload)]
              [name-bytes  (cast name-ptr _pointer _bytes)]
              [name-len    (bytes-length name-bytes)]
              [name        (bytes->path-element name-bytes)]
              [newname-ptr (ptr-add name-ptr (+ 1 name-len))]
              [newname     (bytes->path-element (cast newname-ptr _pointer _bytes))])
         (log-fuse-info "FUSE_RENAME nodeid: ~a name: ~a newnodeid: ~a newname: ~a" nodeid name newnodeid newname)
         (rename #:nodeid nodeid #:name name #:newnodeid newnodeid #:newname newname #:reply reply-empty #:error reply-error))]
      ['FUSE_LINK
       (let* ([link        (filesystem-link (session-filesystem session))]
              [in          (decode-fuse_link_in payload)]
              [oldnodeid   (fuse_link_in-oldnodeid in)]
              [newname-ptr (skip-fuse_link_in payload)]
              [newname     (bytes->path-element (cast newname-ptr _pointer _bytes))])
         (log-fuse-info "FUSE_LINK nodeid: ~a oldnodeid: ~a newname: ~a" nodeid oldnodeid newname)
         (link #:nodeid nodeid #:oldnodeid oldnodeid #:newname newname #:reply reply-entry #:error reply-error))]
      ['FUSE_WRITE ;XXX Should handle FUSE_WRITE_CACHE
       (let* ([write      (filesystem-write (session-filesystem session))]
              [in         (decode-fuse_write_in payload)]
              [info       (fuse_write_in-user in)]
              [offset     (fuse_write_in-offset in)]
              [size       (fuse_write_in-size in)]
              [flags      (fuse_write_in-flags in)]
              [wflags     (fuse_write_in-wflags in)]
              [lockowner  (and (check-flag wflags 'FUSE_WRITE_LOCKOWNER) (fuse_write_in-lckowner in))]
              [data       (make-bytes size)])
         (memcpy data (skip-fuse_write_in payload) size)
         (log-fuse-info "FUSE_WRITE nodeid: ~a offset: ~a data: ~a flags: ~a lockowner: ~a"
                         nodeid offset data flags lockowner)
         (if (check-flag wflags 'FUSE_WRITE_CACHE)
             (reply-error 'ENOSYS)
             (write #:nodeid nodeid #:info info #:offset offset #:data data #:lockowner lockowner
                    #:reply reply-write #:error reply-error)))]
      ['FUSE_RELEASE
       (let* ([release    (filesystem-release (session-filesystem session))]
              [in         (decode-fuse_release_in payload)]
              [info       (fuse_release_in-user in)]
              [flags      (fuse_release_in-flags in)]
              [rflags     (fuse_release_in-rflags in)]
              [flush?     (check-flag rflags 'FUSE_RELEASE_FLUSH)]
              [unlock?    (check-flag rflags 'FUSE_RELEASE_FLOCK_UNLOCK)]
              [lockowner  (fuse_release_in-lckowner in)])
         (log-fuse-info "FUSE_RELEASE nodeid: ~a flags: ~a lockowner: ~a flush: ~a unlock ~a"
                         nodeid flags lockowner flush? unlock?)
         (release #:nodeid nodeid #:info info #:flags flags #:lockowner lockowner #:flush flush? #:unlock unlock?
                  #:reply reply-empty #:error reply-error))]
      ['FUSE_FSYNC
       (let* ([fsync      (filesystem-fsync (session-filesystem session))]
              [in         (decode-fuse_fsync_in payload)]
              [info       (fuse_fsync_in-user in)]
              [flags      (fuse_fsync_in-flags in)]
              [sync?      (check-flag flags 'DATASYNC)])
         (log-fuse-info "FUSE_FSYNC nodeid: ~a syncdataonly: ~a" nodeid sync?)
         (fsync #:nodeid nodeid #:info info #:syncdataonly sync? #:reply reply-empty #:error reply-error))]
      ['FUSE_FLUSH
       (let* ([flush      (filesystem-flush (session-filesystem session))]
              [in         (decode-fuse_flush_in payload)]
              [info       (fuse_flush_in-user in)]
              [lockowner  (fuse_flush_in-lckowner in)])
         (log-fuse-info "FUSE_FLUSH nodeid: ~a lockowner: ~a" info lockowner)
         (flush #:nodeid nodeid #:info info #:lockowner lockowner #:reply reply-empty #:error reply-error))]
      ['FUSE_FSYNCDIR
       (let* ([fsyncdir   (filesystem-fsyncdir (session-filesystem session))]
              [in         (decode-fuse_fsync_in payload)]
              [info       (fuse_fsync_in-user in)]
              [flags      (fuse_fsync_in-flags in)]
              [sync?      (check-flag flags 'DATASYNC)])
         (log-fuse-info "FUSE_FSYNCDIR nodeid: ~a syncdataonly: ~a" nodeid sync?)
         (fsyncdir #:nodeid nodeid #:info info #:syncdataonly sync? #:reply reply-empty #:error reply-error))]
      ['FUSE_ACCESS
       (let* ([access     (filesystem-access (session-filesystem session))]
              [in         (decode-fuse_access_in payload)]
              [mask       (fuse_access_in-mask in)])
         (log-fuse-info "FUSE_ACCESS nodeid: ~a mask: ~a" nodeid mask)
         (access #:nodeid nodeid #:mask mask #:reply reply-empty #:error reply-error))]
      ['FUSE_CREATE
       (let* ([create     (filesystem-create (session-filesystem session))]
              [in         (decode-fuse_create_in payload)]
              [flags      (fuse_create_in-flags in)]
              [mode       (fuse_create_in-mode in)]
              [umask      (fuse_create_in-umask in)]
              [name-ptr   (skip-fuse_create_in payload)]
              [name       (bytes->path-element (cast name-ptr _pointer _bytes))])
         (log-fuse-info "FUSE_CREATE nodeid: ~a name: ~a flags: ~a mode: ~a umask: ~a"
                        nodeid name flags mode umask)
         (create #:nodeid nodeid #:name name #:mode mode #:umask umask #:flags flags
                 #:reply reply-create #:error reply-error))]
      ['FUSE_STATFS
       (let* ([statfs     (filesystem-statfs (session-filesystem session))])
         (log-fuse-info "FUSE_STATFS nodeid: ~a" nodeid)
         (statfs #:nodeid nodeid #:reply reply-statfs #:error reply-error))]
      ['FUSE_SETXATTR
       (let* ([setxattr   (filesystem-setxattr (session-filesystem session))]
              [in         (decode-fuse_setxattr_in payload)]
              [size       (fuse_setxattr_in-size in)]
              [op         (fuse_setxattr_in-op in)]
              [name-ptr   (skip-fuse_setxattr_in payload)]
              [name-bytes (cast name-ptr _pointer _bytes)]
              [name-len   (bytes-length name-bytes)]
              [name       (bytes->string/locale name-bytes)]
              [value-ptr  (ptr-add name-ptr (+ name-len 1))]
              [value      (bytes->string/locale (cast value-ptr _pointer _bytes))])
         (log-fuse-info "FUSE_SETXATTR nodeid: ~a name: ~a value: ~a op: ~a size: ~a")
         (setxattr #:nodeid nodeid #:name name #:value value #:op op #:size size
                   #:reply reply-empty #:error reply-error))]
      ['FUSE_GETXATTR
       (let* ([getxattr   (filesystem-getxattr (session-filesystem session))]
              [in         (decode-fuse_getxattr_in payload)]
              [size       (fuse_getxattr_in-size in)]
              [name-ptr   (skip-fuse_getxattr_in payload)]
              [name       (bytes->string/locale (cast name-ptr _pointer _bytes))])
         (log-fuse-info "FUSE_GETXATTR nodeid: ~a name: ~a size: ~a" nodeid name size)
         (getxattr #:nodeid nodeid #:name name #:reply (make-reply-sized size) #:error reply-error))]
      ['FUSE_LISTXATTR
       (let* ([listxattr  (filesystem-listxattr (session-filesystem session))]
              [in         (decode-fuse_getxattr_in payload)]
              [size       (fuse_getxattr_in-size in)])
         (log-fuse-info "FUSE_LISTXATTR nodeid: ~a size: ~a" nodeid size)
         (listxattr #:nodeid nodeid #:reply (make-reply-sized-list size) #:error reply-error))]
      ['FUSE_REMOVEXATTR
       (let* ([removexattr (filesystem-removexattr (session-filesystem session))]
              [name        (bytes->string/locale (cast payload _pointer _bytes))])
         (log-fuse-info "FUSE_REMOVEXATTR nodeid: ~a name: ~a" nodeid name)
         (removexattr #:nodeid nodeid #:name name #:reply reply-empty #:error reply-error))]
      ['FUSE_GETLK
       (let* ([getlk       (filesystem-getlk (session-filesystem session))]
              [in          (decode-fuse_lk_in payload)]
              [info        (fuse_lk_in-user in)]
              [owner       (fuse_lk_in-owner in)]
              [start       (fuse_lk_in-start in)]
              [end         (fuse_lk_in-end in)]
              [type        (fuse_lk_in-type in)]
              [pid         (fuse_lk_in-pid in)])
         (log-fuse-info "FUSE_GETLK nodeid: ~a owner: ~a start: ~a end: ~a type: ~a pid: ~a"
                        nodeid owner start end type pid)
         (getlk #:nodeid nodeid #:info info #:owner owner #:start start #:end end
                #:type type #:pid pid #:reply reply-lock #:error reply-error))]
      ['FUSE_SETLK
       (let* ([setlk       (filesystem-setlk (session-filesystem session))]
              [in          (decode-fuse_lk_in payload)]
              [info        (fuse_lk_in-user in)]
              [owner       (fuse_lk_in-owner in)]
              [start       (fuse_lk_in-start in)]
              [end         (fuse_lk_in-end in)]
              [flags       (fuse_lk_in-flags in)]
              [type        (or (and (check-flag flags 'FUSE_LK_FLOCK) (cons 'LOCK_NB (fuse_lk_in-type in)))
                               (fuse_lk_in-type in))])
         (log-fuse-info "FUSE_SETLK nodeid: ~a owner: ~a start: ~a end: ~a type: ~a flags: ~a"
                        nodeid owner start end type flags)
         (setlk #:nodeid nodeid #:info info #:owner owner #:start start #:end end #:type type #:sleep #f
                #:reply reply-empty #:error reply-error))]
      ['FUSE_SETLKW
       (let* ([setlk       (filesystem-setlk (session-filesystem session))]
              [in          (decode-fuse_lk_in payload)]
              [info        (fuse_lk_in-user in)]
              [owner       (fuse_lk_in-owner in)]
              [start       (fuse_lk_in-start in)]
              [end         (fuse_lk_in-end in)]
              [flags       (fuse_lk_in-flags in)]
              [type        (fuse_lk_in-type in)])
         (log-fuse-info "FUSE_SETLKW nodeid: ~a owner: ~a start: ~a end: ~a type: ~a flags: ~a"
                        nodeid owner start end type flags)
         (setlk #:nodeid nodeid #:info info #:owner owner #:start start #:end end #:type type #:sleep #t
                #:reply reply-empty #:error reply-error))]
      ['FUSE_BMAP
       (let* ([bmap        (filesystem-bmap (session-filesystem session))]
              [in          (decode-fuse_bmap_in payload)]
              [block       (fuse_bmap_in-block in)]
              [blocksize   (fuse_bmap_in-blocksize in)])
         (log-fuse-info "FUSE_BMAP nodeid: ~a block: ~a blocksize: ~a"
                        nodeid block blocksize)
         (bmap #:nodeid nodeid #:blocksize blocksize #:index block #:reply reply-bmap #:error reply-error))]
      ['FUSE_BATCH_FORGET
       (let* ([forget  (filesystem-forget (session-filesystem session))]
              [in      (decode-fuse_batch_forget_in payload)]
              [count   (fuse_batch_forget_in-count in)])
         (for/fold ([next (skip-fuse_batch_forget_in payload)])
                   ([i    (in-range count)])
           (let* ([entry   (decode-fuse_forget_one next)]
                  [nodeid  (fuse_forget_one-nodeid entry)]
                  [nlookup (fuse_forget_one-nlookup entry)])
             (log-fuse-info "FUSE_BATCH_FORGET nodeid: ~a nlookup: ~a" nodeid nlookup)
             (forget #:nodeid nodeid #:nlookup nlookup)
             (skip-fuse_forget_one next))))]
      ['FUSE_FALLOCATE
       (let* ([fallocate (filesystem-fallocate (session-filesystem session))]
              [in        (decode-fuse_fallocate_in payload)]
              [info      (fuse_fallocate_in-user in)]
              [mode      (fuse_fallocate_in-mode in)]
              [offset    (fuse_fallocate_in-offset in)]
              [length    (fuse_fallocate_in-length in)])
         (log-fuse-info "FUSE_FALLOCATE nodeid: ~a mode: ~a offset: ~a length: ~a" nodeid mode offset length)
         (fallocate #:nodeid nodeid #:info info #:mode mode #:offset offset #:length length
                    #:reply reply-empty #:error reply-error))]
       ['FUSE_READDIRPLUS ;XXX Support readdirplus
        (begin
          (log-fuse-info "FUSE_READDIRPLUS not supported")
          (reply-error 'ENOSYS))]
       ['FUSE_RENAME2 ;XXX Support rename2
        (begin
          (log-fuse-info "FUSE_RENAME2 not supported")
          (reply-error 'ENOSYS))]
      ['FUSE_INTERRUPT ;XXX Support interrupts
       (begin
         (log-fuse-warning "FUSE_INTERRUPT not supported")
         (reply-error 'ENOSYS))]
      ['FUSE_IOCTL ;XXX Support IOCTL
       (begin
         (log-fuse-info "FUSE_IOCTL not supported")
         (reply-error 'ENOSYS))]
      ['FUSE_POLL ;XXX Support poll
       (begin
         (log-fuse-info "FUSE_POLL not support")
         (reply-error 'ENOSYS))]
      ['FUSE_NOTIFY_REPLY ;XXX support notifications
       (begin
         (log-fuse-info "FUSE_NOTIFY_REPLY not supported")
         (reply-error 'ENOSYS))]
      [op
       (begin
         (log-fuse-warning "FUSE operation ~a not supported" op)
         (reply-error 'ENOSYS))])))

(define libfuse (ffi-lib "libfuse"))
(define libc (ffi-lib #f))
(define libracket (ffi-lib #f))

(define-cstruct _fuse_args
  ([argc      _int]
   [argv      (_list i _string)]
   [allocated _int]))

;XXX Remove dependency on libfuse?
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
  (let* ([options (cons "default_permissions" options)]
         [real-path (resolve-path mountpoint)]
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
        buffer)))

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
  (ptr-set! buffer _fuse_out_header (fuse_out_header size code unique))
  (cond
    [(not data)
     (void)]
    [(ctype? ctype-or-size)
     (ptr-set! (skip-fuse_out_header buffer) ctype-or-size data)]
    [else
     (memcpy buffer (ctype-sizeof _fuse_out_header) data data-size)])
  (let ([written (write (channel-fd channel) buffer size)]) ; buffer size
    (unless (eq? size written)
      (let* ([code (saved-errno)]
             [errno (code->errno code)])
        (log-fuse-warning "send failed: ~a: ~a" errno (errno->message errno))
        (when (eq? errno 'EINVAL)
          (raise-user-error 'fuse "send: Invalid argument"))))))
