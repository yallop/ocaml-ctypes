module type Abstract =
sig
  (* Value of abstract types can be passed to and returned from native
     functions, typically by address.  Primitive types can also be
     passed by value.
 *)
  type t
  val t : t Ffi.C.typ
  val is_primitive : bool
end

(* arithmetic types from <sys/types.h> *)

module Blkcnt : Abstract
module Blksize : Abstract
module Clock : Abstract
module Clockid : Abstract
module Dev : Abstract
module Fsblkcnt : Abstract
module Fsfilcnt : Abstract
module Gid : Abstract
module Id : Abstract
module Ino : Abstract
module Mode : Abstract
module Nlink : Abstract
module Off : Abstract
module Pid : Abstract
module Pthread : Abstract
module Size : Abstract with type t = Unsigned.size_t
module Ssize : Abstract
module Suseconds : Abstract
module Time : Abstract
module Uid : Abstract
module Useconds : Abstract

type blkcnt_t = Blkcnt.t
type blksize_t = Blksize.t
type clock_t = Clock.t
type clockid_t = Clockid.t
type dev_t = Dev.t
type fsblkcnt_t = Fsblkcnt.t
type fsfilcnt_t = Fsfilcnt.t
type gid_t = Gid.t
type id_t = Id.t
type ino_t = Ino.t
type mode_t = Mode.t
type nlink_t = Nlink.t
type off_t = Off.t
type pid_t = Pid.t
type pthread_t = Pthread.t
type size_t = Size.t
type ssize_t = Ssize.t
type suseconds_t = Suseconds.t
type time_t = Time.t
type uid_t = Uid.t
type useconds_t = Useconds.t

(* non-arithmetic types from <sys/types.h> *)
module Key : Abstract
module Pthread_Attr : Abstract
module Pthread_Cond : Abstract
module Pthread_Condattr : Abstract
module Pthread_Key : Abstract
module Pthread_Mutex : Abstract
module Pthread_Mutexattr : Abstract
module Pthread_Once : Abstract
module Pthread_Rwlock : Abstract
module Pthread_Rwlockattr : Abstract
module Timer : Abstract

type key_t = Key.t
type pthread_attr_t = Pthread_Attr.t
type pthread_cond_t = Pthread_Cond.t
type pthread_condattr_t = Pthread_Condattr.t
type pthread_key_t = Pthread_Key.t
type pthread_mutex_t = Pthread_Mutex.t
type pthread_mutexattr_t = Pthread_Mutexattr.t
type pthread_once_t = Pthread_Once.t
type pthread_rwlock_t = Pthread_Rwlock.t
type pthread_rwlockattr_t = Pthread_Rwlockattr.t
type timer_t = Timer.t
