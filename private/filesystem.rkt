#lang racket/base

(require racket/contract)

(require "errno.rkt"
         "data.rkt")

(provide
 (contract-out
  [make-filesystem (->* ()
                        (#:init        init/c
                         #:destroy     destroy/c
                         #:lookup      lookup/c
                         #:forget      forget/c
                         #:getattr     getattr/c
                         #:setattr     setattr/c
                         #:readlink    readlink/c
                         #:mknod       mknod/c
                         #:mkdir       mkdir/c
                         #:unlink      unlink/c
                         #:rmdir       rmdir/c
                         #:symlink     symlink/c
                         #:rename      rename/c
                         #:link        link/c
                         #:open        open/c
                         #:read        read/c
                         #:write       write/c
                         #:flush       flush/c
                         #:release     release/c
                         #:fsync       fsync/c
                         #:opendir     opendir/c
                         #:readdir     readdir/c
                         #:releasedir  releasedir/c
                         #:fsyncdir    fsyncdir/c
                         #:statfs      statfs/c
                         #:setxattr    setxattr/c
                         #:getxattr    getxattr/c
                         #:listxattr   listxattr/c
                         #:removexattr removexattr/c
                         #:access      access/c
                         #:create      create/c
                         #:getlk       getlk/c
                         #:setlk       setlk/c
                         #:bmap        bmap/c
                         #:fallocate   fallocate/c)
                        filesystem?)])
 filesystem?
 filesystem-init
 filesystem-destroy
 filesystem-lookup
 filesystem-forget
 filesystem-getattr
 filesystem-setattr
 filesystem-readlink
 filesystem-mknod
 filesystem-mkdir
 filesystem-unlink
 filesystem-rmdir
 filesystem-symlink
 filesystem-rename
 filesystem-link
 filesystem-open
 filesystem-read
 filesystem-write
 filesystem-flush
 filesystem-release
 filesystem-fsync
 filesystem-opendir
 filesystem-readdir
 filesystem-releasedir
 filesystem-fsyncdir
 filesystem-statfs
 filesystem-setxattr
 filesystem-getxattr
 filesystem-listxattr
 filesystem-removexattr
 filesystem-access
 filesystem-create
 filesystem-getlk
 filesystem-setlk
 filesystem-bmap
 filesystem-fallocate
 request-pid
 request-gid
 request-uid)

(struct filesystem
  (init destroy lookup forget getattr setattr readlink mknod mkdir unlink rmdir
   symlink rename link open read write flush release fsync opendir readdir
   releasedir fsyncdir statfs setxattr getxattr listxattr removexattr access
   create getlk setlk bmap fallocate))

(define request-pid (make-parameter #f))
(define request-uid (make-parameter #f))
(define request-gid (make-parameter #f))

(define (make-filesystem
         #:init [init default-init] #:destroy [destroy default-destroy]
         #:lookup [lookup default-lookup] #:forget [forget default-forget]
         #:getattr [getattr default-getattr] #:setattr [setattr default-setattr]
         #:readlink [readlink default-readlink] #:mknod [mknod default-mknod]
         #:mkdir [mkdir default-mkdir] #:unlink [unlink default-unlink]
         #:rmdir [rmdir default-rmdir] #:symlink [symlink default-symlink]
         #:rename [rename default-rename] #:link [link default-link]
         #:open [open default-open] #:read [read default-read]
         #:write [write default-write] #:flush [flush default-flush]
         #:release [release default-release] #:fsync [fsync default-fsync]
         #:opendir [opendir default-opendir] #:readdir [readdir default-readdir]
         #:releasedir [releasedir default-releasedir]
         #:fsyncdir [fsyncdir default-fsyncdir] #:statfs [statfs default-statfs]
         #:setxattr [setxattr default-setxattr]
         #:getxattr [getxattr default-getxattr]
         #:listxattr [listxattr default-listxattr]
         #:removexattr [removexattr default-removexattr]
         #:access [access default-access] #:create [create default-create]
         #:getlk [getlk default-getlk] #:setlk [setlk default-setlk]
         #:bmap [bmap default-bmap] #:fallocate [fallocate default-fallocate])
  (filesystem
   init destroy lookup forget getattr setattr readlink mknod mkdir unlink rmdir
   symlink rename link open read write flush release fsync opendir readdir
   releasedir fsyncdir statfs setxattr getxattr listxattr removexattr access
   create getlk setlk bmap fallocate))

(define (use-once/c ctc)
  (make-contract
   #:name (format "(use-once/c ~a)" (contract-name ctc))
   #:first-order (lambda (v) (and (procedure? v) (contract-first-order-passes? ctc v)))
   #:late-neg-projection
   (lambda (blame)
     (lambda (v neg)
       (let ([full-blame (blame-replace-negative blame neg)]
             [used? #f])
         (unless (procedure? v)
           (raise-blame-error full-blame v
                              '(expected: "~a" given: "~e")
                              "procedure" v))
         (impersonate-procedure
          (((contract-late-neg-projection ctc) blame) v neg)
          (make-keyword-procedure
           (lambda (kwds kw-args . args)
             (when used?
               (raise-blame-error (blame-swap full-blame) v
                                  "use-once procedure invoked multiple times: ~a" v))
             (set! used? #t)
             (apply values kw-args args))
           (lambda args
             (when used?
               (raise-blame-error (blame-swap full-blame) v
                                  "use-once procedure invoked multiple times: ~a" v))
             (set! used? #t)
             (apply values args)))))))))

(define (maybe/c ctc) (or/c ctc #f))

(define reply-error/c (use-once/c (-> errno? void)))

(define init/c (-> (or/c #t errno?)))
(define (default-init)
  #t)

(define destroy/c (-> void))
(define (default-destroy)
  (void))

(define reply-entry/c (use-once/c (-> #:generation uint64?
                                      #:entry-valid timespec? #:attr-valid timespec?
                                      #:inode uint64? #:rdev uint32?
                                      #:size uint64? #:blocks uint64? #:atime timespec?
                                      #:mtime timespec? #:ctime timespec? #:kind filetype?
                                      #:perm perm/c #:nlink uint32? #:uid uint32? #:gid uint32?
                                      void)))
(define lookup/c (-> #:nodeid uint64? #:name path? #:reply reply-entry/c #:error reply-error/c void))
(define (default-lookup #:nodeid node #:name name #:reply reply #:error error)
  (error 'ENOSYS))

(define forget/c (-> #:nodeid uint64? #:nlookup uint64? void))
(define (default-forget #:nodeid node #:nlookup nlookup)
  (void))

(define reply-attr/c (use-once/c (-> #:attr-valid timespec? #:inode uint64? #:rdev uint32?
                                     #:size uint64? #:blocks uint64? #:atime timespec?
                                     #:mtime timespec? #:ctime timespec? #:kind filetype?
                                     #:perm perm/c #:nlink uint32? #:uid uint32? #:gid uint32?
                                     void)))
(define getattr/c (-> #:nodeid uint64? #:info (or/c #f any/c) #:reply reply-attr/c #:error reply-error/c void))
(define (default-getattr #:nodeid node #:info info #:reply reply #:error error)
  (error 'ENOSYS))

(define setattr/c (-> #:nodeid uint64? #:info (or/c #f any/c) #:mode (maybe/c perm/c) #:uid (maybe/c uint32?)
                      #:gid (maybe/c uint32?) #:size (maybe/c uint64?) #:atime (maybe/c timespec?)
                      #:mtime (maybe/c timespec?) #:ctime (maybe/c timespec?)
                      #:reply reply-attr/c #:error reply-error/c void))
(define (default-setattr #:nodeid nodeid #:info info #:mode mode #:uid uid #:gid gid #:size size
          #:atime atime #:mtime mtime #:ctime ctime #:reply reply #:error error)
  (error 'ENOSYS))

(define reply-data/c (use-once/c (-> bytes? void)))

(define readlink/c (-> #:nodeid uint64? #:reply reply-data/c #:error reply-error/c void))
(define (default-readlink #:nodeid nodeid #:reply reply #:error error)
  (error 'ENOSYS))

(define mknod/c (-> #:nodeid uint64? #:name path? #:kind filetype? #:mode perm/c #:umask perm/c #:rdev uint32? #:reply reply-entry/c #:error reply-error/c void))
(define (default-mknod #:nodeid nodeid #:name name #:kind filetype #:mode mode #:umask umask #:rdev rdev #:reply reply #:error error)
  (error 'ENOSYS))

(define mkdir/c (-> #:nodeid uint64? #:name path? #:mode perm/c #:umask perm/c #:reply reply-entry/c #:error reply-error/c void))
(define (default-mkdir #:nodeid uint64? #:name name #:mode mode #:umask umask  #:reply reply #:error error)
  (error 'ENOSYS))

(define reply-empty/c (use-once/c (-> void)))

(define unlink/c (-> #:nodeid uint64? #:name path? #:reply reply-empty/c #:error reply-error/c void))
(define (default-unlink #:nodeid nodeid #:name name #:reply reply #:error error)
  (error 'ENOSYS))

(define rmdir/c (-> #:nodeid uint64? #:name path? #:reply reply-empty/c #:error reply-error/c void))
(define (default-rmdir #:nodeid nodeid #:name name #:reply reply #:error error)
  (error 'ENOSYS))

(define symlink/c (-> #:nodeid uint64? #:name path? #:link path? #:reply reply-entry/c #:error reply-error/c void))
(define (default-symlink #:nodeid nodeid #:name name #:link link #:reply reply #:error error)
  (error 'ENOSYS))

(define rename/c (-> #:nodeid uint64? #:name path? #:newnodeid uint64? #:newname path? #:reply reply-empty/c #:error reply-error/c void))
(define (default-rename #:nodeid nodeid #:name name #:newnodeid newnodeid #:newname newname #:reply reply #:error error)
  (error 'ENOSYS))

(define link/c (-> #:nodeid uint64? #:oldnodeid uint64? #:newname path? #:reply reply-entry/c #:error reply-error/c void))
(define (default-link #:nodeid nodeid #:oldnodeid newparent #:newname newname #:reply reply #:error error)
  (error 'ENOSYS))

(define reply-open/c (use-once/c (-> #:info any/c #:flags open-out-flags/c void)))

(define open/c (-> #:nodeid uint64? #:flags oflags/c #:reply reply-open/c #:error reply-error/c void))
(define (default-open #:nodeid uint64? #:flags flags #:reply reply #:error error)
  (reply #:info #f #:flags (list)))

(define read/c (-> #:nodeid uint64? #:info (or/c #f any/c) #:offset uint64? #:size uint32? #:flags (or/c #f oflags/c) #:lockowner (or/c #f uint64?)
                   #:reply reply-data/c #:error reply-error/c void))
(define (default-read #:nodeid uint64? #:info info #:offset offset #:size size #:flags flags #:lockowner lockowner #:reply reply #:error error)
  (error 'ENOSYS))

(define reply-write/c (use-once/c (-> #:written uint32? void)))

(define write/c (-> #:nodeid uint64? #:info (or/c #f any/c) #:offset uint64? #:data bytes? #:flags oflags/c #:lockowner (or/c #f uint64?)
                    #:reply reply-write/c #:error reply-error/c void))
(define (default-write #:nodeid nodeid #:info info #:offset offset #:data data #:flags flags #:lockowner lockowner #:reply reply #:error error)
  (error 'ENOSYS))

(define flush/c (-> #:nodeid uint64? #:info (or/c #f any/c) #:lockowner uint64? #:reply reply-empty/c #:error reply-error/c void))
(define (default-flush #:nodeid nodeid #:info info #:lockowner lockowner #:reply reply #:error error)
  (error 'ENOSYS))

(define release/c (-> #:nodeid uint64? #:info (or/c #f any/c) #:flags oflags/c #:lockowner uint64? #:flush boolean? #:unlock boolean?
                      #:reply reply-empty/c #:error reply-error/c void))
(define (default-release #:nodeid nodeid #:info info #:flags flags #:lockowner lockowner #:flush flush? #:unlock unlock?
          #:reply reply #:error error)
  (reply))

(define fsync/c (-> #:nodeid uint64? #:info (or/c #f any/c) #:syncdataonly boolean? #:reply reply-empty/c #:error reply-error/c void))
(define (default-fsync #:nodeid uint64? #:info info #:syncdataonly sync? #:reply reply #:error error)
  (error 'ENOSYS))

(define opendir/c (-> #:nodeid uint64? #:flags oflags/c #:reply reply-open/c #:error reply-error/c void))
(define (default-opendir #:nodeid nodeid #:flags flags #:reply reply #:error error)
  (reply #:info #f #:flags (list)))

(define reply-add/c (-> #:inode uint64? #:offset uint64? #:kind filetype? #:name path? boolean?))
(define reply-done/c (use-once/c (-> void)))

(define readdir/c (-> #:nodeid uint64? #:info (or/c #f any/c) #:offset uint64? #:add reply-add/c #:reply reply-done/c #:error reply-error/c void))
(define (default-readdir #:nodeid nodeid #:info info #:offset offset #:add add #:reply reply #:error error)
  (error 'ENOSYS))

(define releasedir/c (-> #:nodeid uint64? #:info (or/c #f any/c) #:flags oflags/c #:reply reply-empty/c #:error reply-error/c void))
(define (default-releasedir #:nodeid nodeid #:info info #:flags flags #:reply reply #:error error)
  (reply))

(define fsyncdir/c (-> #:nodeid uint64? #:info (or/c #f any/c) #:syncdataonly boolean? #:reply reply-empty/c #:error reply-error/c void))
(define (default-fsyncdir #:nodeid nodeid #:info info #:syncdataonly sync? #:reply reply #:error error)
  (error 'ENOSYS))

(define reply-statfs/c (use-once/c (-> #:blocks uint64? #:bfree uint64? #:bavail uint64? #:files uint64? #:ffree uint64? #:bsize uint32? #:namlen uint32? #:frsize uint32? void)))

(define statfs/c (-> #:nodeid uint64? #:reply reply-statfs/c #:error reply-error/c void))
(define (default-statfs #:nodeid nodeid #:reply reply #:error error)
  (error 'ENOSYS))

(define setxattr/c (-> #:nodeid uint64? #:name bytes? #:value bytes? #:op xattr-op? #:size uint64?
                       #:reply reply-empty/c #:error reply-error/c void))
(define (default-setxattr #:nodeid nodeid #:name name #:value value #:op op #:size size #:reply reply #:error error)
  (error 'ENOSYS))

(define getxattr/c (-> #:nodeid uint64? #:name bytes? #:reply reply-data/c #:error reply-error/c void))
(define (default-getxattr #:nodeid nodeid #:name name #:reply reply #:error error)
  (error 'ENOSYS))

(define reply-listxattr/c (use-once/c (-> (listof bytes?) void)))
(define listxattr/c (-> #:nodeid uint64? #:reply reply-listxattr/c #:error reply-error/c void))
(define (default-listxattr #:nodeid nodeid #:reply reply #:error error)
  (error 'ENOSYS))

(define removexattr/c (-> #:nodeid uint64? #:name bytes? #:reply reply-empty/c #:error reply-error/c void))
(define (default-removexattr #:nodeid nodeid #:name name #:reply reply #:error error)
  (error 'ENOSYS))

(define access/c (-> #:nodeid uint64? #:mask perm/c #:reply reply-empty/c #:error reply-error/c void))
(define (default-access #:nodeid nodeid #:mask mask #:reply reply #:error error)
  (error 'ENOSYS))

(define reply-create/c (use-once/c (-> #:generation uint64?
                                       #:entry-valid timespec? #:attr-valid timespec?
                                       #:inode uint64? #:rdev uint32?
                                       #:size uint64? #:blocks uint64? #:atime timespec?
                                       #:mtime timespec? #:ctime timespec? #:kind filetype?
                                       #:perm perm/c #:nlink uint32? #:uid uint32? #:gid uint32?
                                       #:info (or/c #f any/c) #:flags open-out-flags/c
                                      void)))
(define create/c (-> #:nodeid uint64? #:name path? #:mode perm/c #:umask perm/c #:flags oflags/c
                     #:reply reply-create/c #:error reply-error/c void))
(define (default-create #:nodeid nodeid #:name name #:mode mode #:umask umask #:flags flags #:reply reply #:error error)
  (error 'ENOSYS))

(define reply-lock/c (use-once/c (-> #:type lock-types/c #:whence lock-whence? #:start uint64? #:length uint64? #:pid uint64? void)))

(define getlk/c (-> #:nodeid uint64? #:info (or/c #f any/c) #:owner uint64? #:start uint64? #:end uint64?
                    #:type lock-types/c #:pid uint64? #:reply reply-lock/c #:error reply-error/c void))
(define (default-getlk #:nodeid nodeid #:info info #:owner owner #:start start #:end end
          #:type type #:pid pid #:reply reply #:error error)
  (error 'ENOSYS))

(define setlk/c (-> #:nodeid uint64? #:info (or/c #f any/c) #:owner uint64? #:start uint64? #:end uint64?
                    #:type lock-type? #:sleep boolean? #:reply reply-empty/c #:error reply-error/c void))
(define (default-setlk #:nodeid nodeid #:info info #:owner owner #:start start #:end end #:type type #:sleep sleep
          #:reply reply #:error error)
  (error 'ENOSYS))

(define reply-bmap/c (use-once/c (-> #:index uint64? void)))

(define bmap/c (-> #:nodeid uint64? #:blocksize uint32? #:index uint64? #:reply reply-bmap/c #:error reply-error/c void))
(define (default-bmap #:nodeid nodeid #:blocksize size #:index index #:reply reply #:error error)
  (error 'ENOSYS))

(define fallocate/c (-> #:nodeid uint64? #:info (or/c #f any/c) #:mode fallocate-mode?
                        #:offset uint64? #:length uint64? #:reply reply-empty/c #:error reply-error/c void))
(define (default-fallocate #:nodeid nodeid #:info info #:mode mode #:offset offset #:length length
          #:reply reply #:error error)
  (error 'ENOSYS))
