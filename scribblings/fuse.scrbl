#lang scribble/manual
@require[@for-label[fuse
                    racket/base
		    racket/contract
                    racket/path]]

@require[racket/string
         racket/sandbox
         scribble/eval
         @for-syntax[racket/base
                     racket/syntax
                     syntax/parse]]

@title{fuse}
@author{Scott Moore}

@defmodule[fuse]

A Racket library for implementing Filesystems in Userspace (FUSE).

This package implements communication routines for interfacing with the Linux FUSE kernel driver.
Client programs can implement a userspace filesystem by providing a collection of functions that
implement filesystem operations. Currently, racket-fuse depends on the libfuse C library to mount
a FUSE filesystem. However, the communication protocol with the kernel and filesystem API does
not reuse libfuse functionality, and this dependency will be removed in a future release.

@section{Creating a filesystem}

To mount a userspace filesystem, call @racket[mount-filesystem] with a filesystem struct created
by @racket[make-filesystem]. @racket[mount-filesystem] will not return until the filesystem is
umounted.

To umount a userspace filesystem, invoke the program @code{fusermount}:
@code{fusermount -u /path/to/filesystem/mount}. In exceptional circumstances (i.e., when using a
buggy userspace filesystem), you may need to force the filesystem to unmount using the
@code{umount} utility.

@defproc[(mount-filesystem [filesystem filesystem?] [path path?] [options (listof string?)]) void]{
Mounts the userspace filesystem @racket[filesystem] at @racket[path] with FUSE options @racket[options].
Does not return until the filesystem is unmounted. See @code{fuse(8)} for a list of permitted options.
}

@defform/subs[#:kind "procedure" #:id make-filesystem
  (make-filesystem operation ...)
 ([operation (code:line #:init init)
             (code:line #:destroy destroy)
             (code:line #:lookup lookup)
             (code:line #:forget forget)
             (code:line #:getattr getattr)
             (code:line #:setattr setattr)
             (code:line #:readlink readlink)
             (code:line #:mknod mknod)
             (code:line #:mkdir mkdir)
             (code:line #:unlink unlink)
             (code:line #:rmdir rmdir)
             (code:line #:symlink symlink)
             (code:line #:rename rename)
             (code:line #:link link)
             (code:line #:access access)
             (code:line #:open open)
             (code:line #:create create)
             (code:line #:read read)
             (code:line #:write write)
             (code:line #:flush flush)
             (code:line #:release release)
             (code:line #:fsync fsync)
             (code:line #:opendir opendir)
             (code:line #:readdir readdir)
             (code:line #:releasedir releasedir)
             (code:line #:fsyncdir fsyncdir)
             (code:line #:statfs statfs)
             (code:line #:setxattr setxattr)
             (code:line #:getxattr getxattr)
             (code:line #:listxattr listxattr)
             (code:line #:removexattr removexattr)
             (code:line #:getlk getlk)
             (code:line #:setlk setlk)
             (code:line #:bmap bmap)
             (code:line #:fallocate fallocate)])]{
Creates a @racket[filesystem?] struct for use with @racket[mount-filesystem]. Each operation specifies
a procedure that will be invoked when the corresponding filesystem operation is requested by the operating
system. The default implementation of each operation does nothing and replies with the error @racket['ENOSYS].
The specification for the procedure required for each operation is given below. Most hooks provide
two response procedures: one marked @racket[#:reply] to indicate success, and one marked @racket[#:error]
to indicate failure. The response specifications are given below, following descriptions of each operation.
A filesystem can defer responding to an operation by spawning a thread which will call
a response procedure when the operation completes. Each operation should respond only once by calling the
appropriate procedure.
}

@section{Operations}

@defproc[(init) (or/c #t errno?)]{
Called before any other filesystem operation. Use this to initialize any state required by the filesystem.
Return true on success or an errno symbol on error.
}

@defproc[(destroy) void]{
Called on filesystem exit.
}

@defproc[(lookup [#:nodeid nodeid uint64?]
                 [#:name name path-element?]
                 [#:reply reply reply-entry/c]
                 [#:error error reply-error/c]) void]{
Lookup the entry @racket[name] in the directory with inode @racket[nodeid], returning its attributes.

Possible responses are @racket[reply-entry] and @racket[reply-error].
}

@defproc[(forget [#:nodeid nodeid uint64?]
                 [#:nlookup nlookup uint64?]) void]{
Each invocation of a @racket[reply-entry] @racket[reply-create] response increments the lookup
count for the corresponding inode by one. Each time a reference to the inode is removed from the kernel's
cache, the lookup count is decremented. Subsequently, @racket[forget] on the FUSE filesystem is invoked
with @racket[nlookup] set to the number of lookups to forget.

While an inode has a non-zero lookup count, the filesystem may receive requests to access the node (even following
calls to unlink, rmdir, etc.). A filesystem should retain the underlying resources associated with the inode until
the lookup count is zero (because a sufficient number of @racket[forget] messages have been received).

If a file has been exported over NFS, the resources should be retained even longer. See the libfuse documentation
for more details.

@racket[forget] may not be invoked on all active indoes when the filesystem is unmounted.
}

@defproc[(getattr [#:nodeid nodeid uint64?]
                  [#:info info uint64?]
                  [#:reply reply reply-attr/c]
                  [#:error error reply-error/c]) void]{}

@defproc[(setattr [#:nodeid nodeid uint64?]
                  [#:info info uint64?]
                  [#:mode mode (maybe/c perm/c)]
                  [#:uid uid (maybe/c uint32?)]
                  [#:gid gid (maybe/c uint32?)]
                  [#:size size (maybe/c uint64?)]
                  [#:atime atime (maybe/c timespec?)]
                  [#:mtime mtime (maybe/c timespec?)]
                  [#:ctime ctime (maybe/c timespec?)]
                  [#:reply reply reply-attr/c]
                  [#:error error reply-error/c]) void]{}

@defproc[(readlink [#:nodeid nodeid uint64?]
                   [#:reply reply reply-data/c]
                   [#:error error reply-error/c]) void]{}

@defproc[(mknod [#:nodeid nodeid uint64?]
                [#:name path path?]
                [#:kind kind filetype?]
                [#:mode mode perm/c]
                [#:umask umask perm/c]
                [#:rdev rdev uint32?]
                [#:reply reply reply-entry/c]
                [#:error error reply-error/c]) void]{}

@defproc[(mkdir [#:nodeid nodeid uint64?]
                [#:name name path-element?]
                [#:mode mode perm/c]
                [#:umask umask perm/c]
                [#:reply reply reply-entry/c]
                [#:error error reply-error/c]) void]{}

@defproc[(unlink [#:nodeid nodeid uint64?]
                 [#:name name path-element?]
                 [#:reply reply reply-empty/c]
                 [#:error error reply-error/c]) void]{}

@defproc[(rmdir [#:nodeid nodeid uint64?]
                [#:name name path-element?]
                [#:reply reply reply-empty/c]
                [#:error error reply-error/c]) void]{}

@defproc[(symlink [#:nodeid nodeid uint64?]
                  [#:name name path-element?]
                  [#:link link path?]
                  [#:reply reply reply-entry/c]
                  [#:error error reply-error/c]) void]{}

@defproc[(rename [#:nodeid nodeid uint64?]
                 [#:name name path-element?]
                 [#:newnodeid newparent uint64?]
                 [#:newname newname path-element?]
                 [#:reply reply reply-empty/c]
                 [#:error error reply-error/c]) void]{}

@defproc[(link [#:nodeid nodeid uint64?]
               [#:oldnodeid oldnodeid uint64?]
               [#:newname newname path-element?]
               [#:reply reply reply-entry/c]
               [#:error error reply-error/c]) void]{}

@defproc[(access [#:nodeid nodeid uint64?]
                 [#:mask mask perm/c]
                 [#:reply reply reply-empty/c]
                 [#:error error reply-error/c]) void]{}

@defproc[(open [#:nodeid nodeid uint64?]
               [#:flags flags oflags/c]
               [#:reply reply reply-open/c]
               [#:error error reply-error/c]) void]{}

@defproc[(create [#:nodeid nodeid uint64?]
                 [#:name name path-element?]
                 [#:mode mode perm/c]
                 [#:umask umask perm/c]
                 [#:flags flags oflags/c]
                 [#:reply reply reply-create/c]
                 [#:error error reply-error/c]) void]{}

@defproc[(read [#:nodeid nodeid uint64?]
               [#:info info uint64?]
               [#:offset offset uint64?]
               [#:size size uint32?]
               [#:lockowner owner (or/c #f uint64?)]
               [#:reply reply reply-data/c]
               [#:error error reply-error/c]) void]{}

@defproc[(write [#:nodeid nodeid uint64?]
                [#:info info uint64?]
                [#:offset offset uint64?]
                [#:data data bytes?]
                [#:lockowner owner (or/c #f uint64?)]
                [#:reply reply reply-write/c]
                [#:error error reply-error/c]) void]{}

@defproc[(flush) void]{}

@defproc[(release) void]{}

@defproc[(fsyncdir) void]{}

@defproc[(statfs) void]{}

@defproc[(setxattr) void]{}

@defproc[(getxattr) void]{}

@defproc[(listxattr) void]{}

@defproc[(removexattr) void]{}

@defproc[(getlk) void]{}

@defproc[(setlk) void]{}

@defproc[(bmap) void]{}

@defproc[(fallocate) void]{}

@section{Responses}

@defthing[reply-error/c contract?]{}

@defthing[reply-empty/c contract?]{}

@defthing[reply-entry/c contract?]{}

@defthing[reply-attr/c contract?]{}

@section{Miscellaneous}

@defstruct[timespec ([sec (exact-positive-integer?)] [nsec (exact-positive-integer?)])]{
A time expressed as seconds (and nanoseconds) the since POSIX epoch, 1970-01-01 00:00:00 +0000 (UTC).
}

@require[@for-syntax[fuse/private/data
                     fuse/private/errno]]

@(define-syntax (code-eval stx)
  (syntax-parse stx
   [(_ symbols)
    (with-syntax ([(sym ...) (eval #'symbols)])
     #'(racket '(sym ...)))]))

@(define-syntax (defenum stx)
  (syntax-parse stx
   [(_ name)
    (with-syntax ([pred  (format-id #'name "~a?" #'name)]
                  [symbols (format-id #'name "~a-symbols" #'name)])
    #'@defproc[(pred [v any/c]) boolean?]{Checks if @racket[v] is either a member of @code-eval[symbols].})]))

@(define-syntax (defbitmask stx)
  (syntax-parse stx
   [(_ name)
    (with-syntax ([ctc  (format-id #'name "~as/c" #'name)]
                  [symbols (format-id #'name "~a-symbols" #'name)])
    #'@defproc[(ctc [v any/c]) boolean?]{Checks if @racket[v] is either a member of or a sublist of @code-eval[symbols].})]))

@defenum[errno]
@defbitmask[mode]
@defbitmask[oflag]
