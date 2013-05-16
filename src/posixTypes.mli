open Ffi.C

module type Abstract =
sig
  (* Value of abstract types can be passed to and returned from native
     functions, typically by address.  Primitive types can also be
     passed by value.
  *)
  type t
  val t : t Ffi.C.typ
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

val blkcnt_t : blkcnt_t typ
val blksize_t : blksize_t typ
val clock_t : clock_t typ
val clockid_t : clockid_t typ
val dev_t : dev_t typ
val fsblkcnt_t : fsblkcnt_t typ
val fsfilcnt_t : fsfilcnt_t typ
val gid_t : gid_t typ
val id_t : id_t typ
val ino_t : ino_t typ
val mode_t : mode_t typ
val nlink_t : nlink_t typ
val off_t : off_t typ
val pid_t : pid_t typ
val pthread_t : pthread_t typ
val size_t : size_t typ
val ssize_t : ssize_t typ
val suseconds_t : suseconds_t typ
val time_t : time_t typ
val uid_t : uid_t typ
val useconds_t : useconds_t typ

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

val key_t : key_t typ
val pthread_attr_t : pthread_attr_t typ
val pthread_cond_t : pthread_cond_t typ
val pthread_condattr_t : pthread_condattr_t typ
val pthread_key_t : pthread_key_t typ
val pthread_mutex_t : pthread_mutex_t typ
val pthread_mutexattr_t : pthread_mutexattr_t typ
val pthread_once_t : pthread_once_t typ
val pthread_rwlock_t : pthread_rwlock_t typ
val pthread_rwlockattr_t : pthread_rwlockattr_t typ
val timer_t : timer_t typ
