#lang scribble/manual

@require[@for-label[fuse
                    racket/base
		    racket/contract
                    racket/path]]

@require[racket/string
         racket/file
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

See @secref{example} for an example FUSE filesystem.

@section{Creating a filesystem}

To mount a userspace filesystem, call @racket[mount-filesystem] with a filesystem struct created
by @racket[make-filesystem]. The @racket[mount-filesystem] procedure will not return until the filesystem is
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

Filesystem procedures can access information about the process making a request using
the parameters @racket[request-pid], @racket[request-uid], @racket[request-gid].
}

@section{Operations}

The following documentation was adapted from the reference low level FUSE API, available at
@link["https://github.com/libfuse/libfuse/blob/master/include/fuse_lowlevel.h"]{https://github.com/libfuse/libfuse/blob/master/include/fuse_lowlevel.h}.

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

Operation @racket[forget] may not be invoked on all active indoes when the filesystem is unmounted.
}

@defproc[(getattr [#:nodeid nodeid uint64?]
                  [#:info info (maybe/c uint64?)]
                  [#:reply reply reply-attr/c]
                  [#:error error reply-error/c]) void]{
Get the file attributes of @racket[nodeid]. The @racket[info] argument may in future contain
information provided by the filesystem when opening the node, but currently
is always @racket[#f].
}

@defproc[(setattr [#:nodeid nodeid uint64?]
                  [#:info info uint64?]
                  [#:mode mode (maybe/c perms/c)]
                  [#:uid uid (maybe/c uint32?)]
                  [#:gid gid (maybe/c uint32?)]
                  [#:size size (maybe/c uint64?)]
                  [#:atime atime (maybe/c timespec?)]
                  [#:mtime mtime (maybe/c timespec?)]
                  [#:ctime ctime (maybe/c timespec?)]
                  [#:reply reply reply-attr/c]
                  [#:error error reply-error/c]) void]{
Set the file attributes of @racket[nodeid]. Each argument is
either @racket[#f] if the corresponding attribute should not
be modified, or contains the new value for that attribute.
If @racket[setattr] was invoked by the @code{ftruncate()}
system call (and the kernel is version 2.6.15 or later),
@racket[info] will contain the value set by the filesystem's @racket[open] procedure.
Otherwise, it will be set to 0.

On success, the filesystem should respond with the updated file attributes.
}

@defproc[(readlink [#:nodeid nodeid uint64?]
                   [#:reply reply reply-data/c]
                   [#:error error reply-error/c]) void]{
Read the symbolic link at inode @racket[nodeid].
}

@defproc[(mknod [#:nodeid nodeid uint64?]
                [#:name path path?]
                [#:kind kind filetype?]
                [#:mode mode perms/c]
                [#:umask umask perms/c]
                [#:rdev rdev uint32?]
                [#:reply reply reply-entry/c]
                [#:error error reply-error/c]) void]{
Create a regular file, character device, fifo, or socket in the
directory @racket[nodeid] with name @racket[name]. The desired filetype
is given by @racket[kind] along with the desired @racket[mode] and
current @racket[umask]. If the filetype is a device type, the desired
device number is given by @racket[rdev].
}

@defproc[(mkdir [#:nodeid nodeid uint64?]
                [#:name name path-element?]
                [#:mode mode perms/c]
                [#:umask umask perms/c]
                [#:reply reply reply-entry/c]
                [#:error error reply-error/c]) void]{
Create a directory with name @racket[name] in directory @racket[nodeid].
The requested file mode and current process umask are given by @racket[mode]
and @racket[umask].
}

@defproc[(unlink [#:nodeid nodeid uint64?]
                 [#:name name path-element?]
                 [#:reply reply reply-empty/c]
                 [#:error error reply-error/c]) void]{
Remove the file with name @racket[name] from directory @racket[nodeid].
If the file's lookup count is non-zero, the file system should retain
any resources required to respond to operations on the file until the
lookup count reaches zero. (See @racket[forget]).
}

@defproc[(rmdir [#:nodeid nodeid uint64?]
                [#:name name path-element?]
                [#:reply reply reply-empty/c]
                [#:error error reply-error/c]) void]{
Remove the directory with the name @racket[name] from directory
@racket[nodeid]. If the directory's lookup count is non-zero, the file
system should retain any resources required to respond to operations on
the directory until the lookup count reaches zero. (See @racket[forget]).
}

@defproc[(symlink [#:nodeid nodeid uint64?]
                  [#:name name path-element?]
                  [#:link link path?]
                  [#:reply reply reply-entry/c]
                  [#:error error reply-error/c]) void]{
Create a symbolic link in directory @racket[nodeid] with name @racket[name]
and content @racket[link].
}

@defproc[(rename [#:nodeid nodeid uint64?]
                 [#:name name path-element?]
                 [#:newnodeid newparent uint64?]
                 [#:newname newname path-element?]
                 [#:reply reply reply-empty/c]
                 [#:error error reply-error/c]) void]{
Move the directory entry @racket[name] in @racket[nodeid] to
@racket[newname] in @racket[newparent]. If an entry @racket[newname] in
@racket[newparent] already exists, it should be atomically replaced.
If the replaced entry's lookup count is non-zero, the file
system should retain any resources required to respond to operations on
the entry until the lookup count reaches zero. (See @racket[forget]).
}

@defproc[(link [#:nodeid nodeid uint64?]
               [#:newparent newparent uint64?]
               [#:newname newname path-element?]
               [#:reply reply reply-entry/c]
               [#:error error reply-error/c]) void]{
Create a hard link to @racket[nodeid] in the directory @racket[newparent]
with name @racket[newname].
}

@defproc[(access [#:nodeid nodeid uint64?]
                 [#:mask mask perms/c]
                 [#:reply reply reply-empty/c]
                 [#:error error reply-error/c]) void]{
Check file access permissions. If the 'default_permissions' mount option is
given, this method is not called. (See the
@link["https://github.com/libfuse/libfuse"]{libfuse documentation}
for important security information related to FUSE file access permissions).
}

@defproc[(open [#:nodeid nodeid uint64?]
               [#:flags flags oflags/c]
               [#:reply reply reply-open/c]
               [#:error error reply-error/c]) void]{
Open the file at inode @racket[nodeid]. Argument @racket[flags] records the open flags
passed to the originating system call, except for @racket['O_CREAT], @racket[O_EXCL],
@racket[O_NOCTTY], and @racket[O_TRUNC], which are handled by FUSE by invoking
@racket[create] instead of @racket[open]. When responding to an @racket[open] operation,
the filesystem can specify a value that will be passed to the filesystem as the @racket[#:info]
associated with an operation on the opened file.
}

@defproc[(create [#:nodeid nodeid uint64?]
                 [#:name name path-element?]
                 [#:mode mode perms/c]
                 [#:umask umask perms/c]
                 [#:flags flags oflags/c]
                 [#:reply reply reply-create/c]
                 [#:error error reply-error/c]) void]{
Create and open a file in the directory @racket[nodeid] with name @racket[name].
Argument @racket[flags] lists the flags passed to the originating @racket[open] system call.
When responding to an @racket[create] operation, the filesystem can specify a value
that will be passed to the filesystem as the @racket[#:info] associated with an
operation on the created file.
}

@defproc[(read [#:nodeid nodeid uint64?]
               [#:info info uint64?]
               [#:offset offset uint64?]
               [#:size size uint32?]
               [#:lockowner owner (or/c #f uint64?)]
               [#:reply reply reply-data/c]
               [#:error error reply-error/c]) void]{
Read @racket[size] bytes from the file at offset @racket[offset]. Read should respond
with exactly the number of bytes requested (except on EOF or error), unless the
@racket['FOPEN_DIRECT_IO] flag was included in the response when opening the file or
the filesystem was mounted using the option @racket["direct_io"]. If direct IO is being used,
the system call result for @racket[read] will reflect the number of bytes returned by
the filesystem's response.
}

@defproc[(write [#:nodeid nodeid uint64?]
                [#:info info uint64?]
                [#:offset offset uint64?]
                [#:data data bytes?]
                [#:lockowner owner (or/c #f uint64?)]
                [#:reply reply reply-write/c]
                [#:error error reply-error/c]) void]{
Unless the file was opened with the @racket['FOPEN_DIRECT_IO] flag or the filesystem
was mounted with the option @racket["direct_io"], write the entire request @racket[data]
to the file at offset @racket[offset]. If direct_io is being used, the operation may
write fewer bytes and return the number of bytes written.
}

@defproc[(flush [#:nodeid nodeid uint64?]
                [#:info info uint64?]
                [#:lockowner owner uint64?]
                [#:reply reply reply-empty/c]
                [#:error error reply-error/c]) void]{
Called whenever the @racket[close] system call is invoked on a file. May be invoked zero or
multiple times per @racket[open] call depending on whether the file is closed and if the file
descriptor of an open file has been duplicated.

If the filesystem supports locking, it should remove all locks belonging to @racket[owner].
}

@defproc[(release [#:nodeid nodeid uint64?]
                  [#:info info uint64?]
                  [#:lockowner owner uint64?]
                  [#:flush flush boolean?]
                  [#:unlock unlock boolean?]
                  [#:reply reply reply-empty/c]
                  [#:error error reply-error/c]) void]{
Invoked when there are no outstanding file descriptors or memory mappings for a file. Will be
called exactly once per @racket[open] call.
}

@defproc[(fsyncdir [#:nodeid nodeid uint64?]
                   [#:info info uint64?]
                   [#:syncdataonly syncdata boolean?]
                   [#:reply reply reply-empty/c]
                   [#:error error reply-error/c]) void]{
Synchronize the contents of directory @racket[nodeid]. If @racket[syncdata] is @racket[#t],
synchronize only the contents of the directory, and not meta data.
}

@defproc[(statfs [#:nodeid nodeid uint64?]
                 [#:reply reply reply-statfs/c]
                 [#:error error reply-error/c]) void]{
Return filesystem statistics for the filesystem containing @racket[nodeid].
}

@defproc[(setxattr [#:nodeid nodeid uint64?]
                   [#:name name bytes?]
                   [#:value value bytes?]
                   [#:op op xattr-op?]
                   [#:size size uint64?]
                   [#:reply reply reply-empty/c]
                   [#:error error reply-error/c]) void]{
Set the extended attribute @racket[name] on file @racket[nodeid] to @racket[value].
Argument @racket[op] is one of @racket['XATTR_DEFAULT], @racket['XATTR_CREATE], or @racket['XATTR_REPLACE].
If @racket[op] is @racket['XATTR_DEFAULT], the extended attribute should be created if it does not exist
or, if it exists the value should be replaced. If @racket[op] is @racket['XATTR_CREATE], the extended
attribute should be created if it does not exist, otherwise the operation should fail. If @racket[op] is
@racket['XATTR_REPLACE], the operation should fail if the extended attribute does not exists, otherwise
it should replce the current value.
}

@defproc[(getxattr [#:nodeid nodeid uint64?]
                   [#:name name bytes?]
                   [#:reply reply reply-data/c]
                   [#:error error reply-error/c]) void]{
Retreive the extended attribute with name @racket[name] for file @racket[nodeid].
}

@defproc[(listxattr [#:nodeid nodeid uint64?]
                    [#:reply reply reply-listxattr/c]
                    [#:error error reply-error/c]) void]{
Retreive the names of all currently set extended attributes for @racket[nodeid].
}

@defproc[(removexattr [#:nodeid nodeid uint64?]
                      [#:name name bytes?]
                      [#:reply reply reply-empty/c]
                      [#:error error reply-error/c]) void]{
Remove the extended attribute @racket[name] for file @racket[nodeid].
}

@defproc[(getlk [#:nodeid nodeid uint64?]
                [#:info info uint64?]
                [#:owner owner uint64?]
                [#:start start uint64?]
                [#:end end uint64?]
                [#:type type lock-types/c]
                [#:pid pid uint64?]
                [#:reply reply reply-lock/c]
                [#:error error reply-error/c]) void]{
Test for the existence of a POSIX lock on @racket[nodeid]. According to libfuse,
if @racket[getlk] and @racket[setlk] are not implemented, the kernel will implement
file locking automatically, so these operations are only required in special cases
(for example, to enable network filesystems to correctly implement distributed locking).
}

@defproc[(setlk [#:nodeid nodeid uint64?]
                [#:info info uint64?]
                [#:owner owner uint64?]
                [#:start start uint64?]
                [#:end end uint64?]
                [#:type type lock-types/c]
                [#:sleep sleep boolean?]
                [#:reply reply reply-empty/c]
                [#:error error reply-error/c]) void]{
Acquire, modify, or release a POSIX lock on @racket[nodeid]. According to libfuse,
if @racket[getlk] and @racket[setlk] are not implemented, the kernel will implement
file locking automatically, so these operations are only required in special cases
(for example, to enable network filesystems to correctly implement distributed locking).
}

@defproc[(bmap [#:nodeid nodeid uint64?]
               [#:blocksize blocksize uint32?]
               [#:index index uint64?]
               [#:reply reply reply-bmap/c]
               [#:error error reply-error/c]) void]{
Return the device block index for the block with index @racket[index] of file @racket[nodeid].
Only useful for block-device backed filesystems using where the filesystem was mounted with
the @racket["blkdev"] FUSE option.
}

@defproc[(fallocate [#:nodeid nodeid uint64?]
                    [#:info info uint64?]
                    [#:mode mode fallocate-mode?]
                    [#:offset offset uint64?]
                    [#:length length uint64?]
                    [#:reply reply reply-empty/c]
                    [#:error error reply-error/c]) void]{
Allocate the requested space within file @racket[nodeid]. Argument @racket[mode] describes the particular
fallocate operation requested. See @code{fallocate(2)} for more details.
}

@section{Responses}

@defthing[reply-error/c contract?
          #:value (use-once/c (-> errno?
                                  void))]{
Respond to an operation with an error.
}

@defthing[reply-empty/c contract?
          #:value (use-once/c (-> void))]{
Indicates that an operation succeeded without providing any additional information.
}

@defthing[reply-entry/c contract?
          #:value (use-once/c (-> #:generation uint64?
                                  #:entry-valid timespec?
                                  #:attr-valid timespec?
                                  #:inode uint64?
                                  #:rdev uint32?
                                  #:size uint64?
                                  #:blocks uint64?
                                  #:atime timespec?
                                  #:mtime timespec?
                                  #:ctime timespec?
                                  #:kind filetype?
                                  #:perm perms/c
                                  #:nlink uint32?
                                  #:uid uint32?
                                  #:gid uint32?
                                  void))]{
Respond to an operation with information about a filesystem entry.
If the file system is exported as a network file system, @racket[#:generation] and @racket[#:inode] pairs
must be unique over the lifetime of the filesystem. @racket[#:entry-valid] and @racket[#:attr-valid] indicate
how long the kernel may cache the entry and attribute information for the inode. @racket[#:rdev] is the
device number of the device containing the inode. @racket[#:atime], @racket[#:mtime], and @racket[#:ctime]
are the last access, last modification, and creation times of the entry. @racket[#:kind] indicates the
type of filesystem object represented by the entry. @racket[#:perm], @racket[#:uid], and @racket[#:gid] are
the is UNIX file mode, owner, and group of the entry. @racket[#:nlink] reports the number of links to the entry
in the filesystem.
}

@defthing[reply-attr/c contract?
          #:value (use-once/c (-> #:attr-valid timespec?
                                  #:inode uint64?
                                  #:rdev uint32?
                                  #:size uint64?
                                  #:blocks uint64?
                                  #:atime timespec?
                                  #:mtime timespec?
                                  #:ctime timespec?
                                  #:kind filetype?
                                  #:perm perms/c
                                  #:nlink uint32?
                                  #:uid uint32?
                                  #:gid uint32?
                                  void))]{
Respond to an operation with a file's attributes.  @racket[#:attr-valid] indicates how long the kernel may cache the
attribute information for the inode. Argument @racket[#:rdev] is the device number of the device containing the inode.
Arguments @racket[#:atime], @racket[#:mtime], and @racket[#:ctime] are the last access, last modification, and creation times
of the entry. @racket[#:kind] indicates the type of filesystem object represented by the entry. Arguments @racket[#:perm],
@racket[#:uid], and @racket[#:gid] are the is UNIX file mode, owner, and group of the entry. Argument @racket[#:nlink]
reports the number of links to the entry in the filesystem.
}

@defthing[reply-data/c contract?
          #:value (use-once/c (-> bytes?
                                  void))]{
Respond to an operation with a byte array.
}

@defthing[reply-open/c contract?
          #:value (use-once/c (-> #:info any/c
                                  #:flags open-out-flags/c
                                  void))]{
Respond to an @racket[open] operation with a user defined value @racket[#:info] and flags configuring
future operations on the file.
}

@defthing[reply-create/c contract?
          #:value (use-once/c (-> #:generation uint64?
                                  #:entry-valid timespec?
                                  #:attr-valid timespec?
                                  #:inode uint64?
                                  #:rdev uint32?
                                  #:size uint64?
                                  #:blocks uint64?
                                  #:atime timespec?
                                  #:mtime timespec?
                                  #:ctime timespec?
                                  #:kind filetype?
                                  #:perm perms/c
                                  #:nlink uint32?
                                  #:uid uint32?
                                  #:gid uint32?
                                  #:info uint64?
                                  #:flags open-out-flags/c
                                  void))]{
Respond to a @racket[create] operation with a user defined value @racket[#:info] and flags configuring
future operations on the file. In addition, provide information about the newly created entry
and its attributes (see @racket[reply-entry/c] for details).
}

@defthing[reply-write/c contract?
          #:value (use-once/c (-> #:written uint32?
                                  void))]{
Respond to a @racket[write] operation with the number of bytes written.
}

@defthing[reply-statfs/c contract?
          #:value (use-once/c (-> #:blocks uint64?
                                  #:bfree uint64?
                                  #:bavail uint64?
                                  #:files uint64?
                                  #:ffree uint64?
                                  #:bsize uint32?
                                  #:namlen uint32?
                                  #:frsize uint32?
                                  void))]{
Respond to a @racket[statfs] operation with information about a filesystem.
}

@defthing[reply-listxattr/c contract?
          #:value (use-once/c (-> (listof bytes?)
                                  void))]{
Respond to a @racket[listxattr] operation with the list of names of currently set extended attributes.
}

@defthing[reply-lock/c contract?
          #:value (use-once/c (-> #:type lock-types/c
                                  #:whence lock-whence?
                                  #:start uint64?
                                  #:length uint64?
                                  #:pid uint64?
                                  void))]{
Respond to a lock operation with information about a POSIX lock.
}

@defthing[reply-bmap/c contract?
          #:value (use-once/c (-> #:index uint64?
                                  void))]{
Respond to a @racket[bmap] operation with the device block index of a block within a file.
}

@section{Miscellaneous}

@defstruct[timespec
           ([sec (exact-positive-integer?)]
            [nsec (exact-positive-integer?)])
           #:omit-constructor]{
A time expressed as seconds (and nanoseconds) the since POSIX epoch, 1970-01-01 00:00:00 +0000 (UTC).
}

@defparam[request-pid pid uint64? #:value #f]{
The process id of the process that triggered the current filesystem operation.
}

@defparam[request-uid uid uint64? #:value #f]{
The effective user id of the process that triggered the current filesystem operation.
}

@defparam[request-gid gid uint64? #:value #f]{
The effective group id of the process that triggered the current filesystem operation.
}

@defthing[uint32? flat-contract?]{
A flat-contract that requires the input to be an @racket[exact-positive-integer?]
that can be represented by a 32-bit unsigned integer.
}

@defthing[uint64? flat-contract?]{
A flat-contract that requires the input to be an @racket[exact-positive-integer?]
that can be represented by a 64-bit unsigned integer.
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
@defbitmask[perm]
@defenum[filetype]
@defbitmask[oflag]
@defenum[xattr-op]
@defbitmask[lock-type]
@defenum[fallocate-mode]

@section[#:tag "example"]{Example}

The module @racketmodfont{fuse/examples/hello} implements an example FUSE
filesystem. It is reproduced below for convenience. To run the filesystem,
invoke @code{racket -l fuse/examples/hello /path/to/mount/point}.
(@code{/path/to/mount/point} must exist as an empty directory). You can
then explore the mounted filesystem at @code{/path/to/mount/point}. To unmount the
filesystem, invoke @code{fusermount -u /path/to/mount/point}.

@filebox["fuse/examples/hello" @typeset-code[@file->string["examples/hello.rkt"]]]
