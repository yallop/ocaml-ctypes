module type Abstract =
sig
  type t
  val t : t Ffi.C.typ
  val is_primitive : bool
end

let mkAbstract : 'a. 'a Ffi.C.typ -> primitive:bool -> (module Abstract)
  = fun (type a) (ty : a Ffi.C.typ) ~primitive ->
    (module
     struct
       type t = a
       let t = ty
       let is_primitive = primitive
     end : Abstract)

let mkAbstractSized : size:int -> (module Abstract)
  = fun ~size ->
    (module
     struct
       open Ffi.C
       open Type
       open Unsigned
       type t = uchar array
       let t = array size uchar
       let is_primitive = false
     end : Abstract)

type arithmetic =
    Int8
  | Int16
  | Int32
  | Int64
  | Uint8
  | Uint16
  | Uint32
  | Uint64
  | Float
  | Double

let mkArithmetic = 
  let open Ffi.C.Type in
  let mk = mkAbstract ~primitive:true in function
    Int8   -> mk int8_t
  | Int16  -> mk int16_t
  | Int32  -> mk int32_t
  | Int64  -> mk int64_t
  | Uint8  -> mk uint8_t
  | Uint16 -> mk uint16_t
  | Uint32 -> mk uint32_t
  | Uint64 -> mk uint64_t
  | Float  -> mk float
  | Double -> mk double

(* Arithmetic types *)

external typeof_blkcnt_t : unit -> arithmetic = "ctypes_typeof_blkcnt_t"
module Blkcnt = (val mkArithmetic (typeof_blkcnt_t ()) : Abstract)
external typeof_blksize_t : unit -> arithmetic = "ctypes_typeof_blksize_t"
module Blksize = (val mkArithmetic (typeof_blksize_t ()) : Abstract)
external typeof_clock_t : unit -> arithmetic = "ctypes_typeof_clock_t"
module Clock = (val mkArithmetic (typeof_clock_t ()) : Abstract)
external typeof_clockid_t : unit -> arithmetic = "ctypes_typeof_clockid_t"
module Clockid = (val mkArithmetic (typeof_clockid_t ()) : Abstract)
external typeof_dev_t : unit -> arithmetic = "ctypes_typeof_dev_t"
module Dev = (val mkArithmetic (typeof_dev_t ()) : Abstract)
external typeof_fsblkcnt_t : unit -> arithmetic = "ctypes_typeof_fsblkcnt_t"
module Fsblkcnt = (val mkArithmetic (typeof_fsblkcnt_t ()) : Abstract)
external typeof_fsfilcnt_t : unit -> arithmetic = "ctypes_typeof_fsfilcnt_t"
module Fsfilcnt = (val mkArithmetic (typeof_fsfilcnt_t ()) : Abstract)
external typeof_gid_t : unit -> arithmetic = "ctypes_typeof_gid_t"
module Gid = (val mkArithmetic (typeof_gid_t ()) : Abstract)
external typeof_id_t : unit -> arithmetic = "ctypes_typeof_id_t"
module Id = (val mkArithmetic (typeof_id_t ()) : Abstract)
external typeof_ino_t : unit -> arithmetic = "ctypes_typeof_ino_t"
module Ino = (val mkArithmetic (typeof_ino_t ()) : Abstract)
external typeof_mode_t : unit -> arithmetic = "ctypes_typeof_mode_t"
module Mode = (val mkArithmetic (typeof_mode_t ()) : Abstract)
external typeof_nlink_t : unit -> arithmetic = "ctypes_typeof_nlink_t"
module Nlink = (val mkArithmetic (typeof_nlink_t ()) : Abstract)
external typeof_off_t : unit -> arithmetic = "ctypes_typeof_off_t"
module Off = (val mkArithmetic (typeof_off_t ()) : Abstract)
external typeof_pid_t : unit -> arithmetic = "ctypes_typeof_pid_t"
module Pid = (val mkArithmetic (typeof_pid_t ()) : Abstract)
external typeof_pthread_t : unit -> arithmetic = "ctypes_typeof_pthread_t"
module Pthread = (val mkArithmetic (typeof_pthread_t ()) : Abstract)
module Size = 
struct
  type t = Unsigned.size_t
  let t = Ffi.C.Type.size_t
  let is_primitive = true
end
external typeof_ssize_t : unit -> arithmetic = "ctypes_typeof_ssize_t"
module Ssize = (val mkArithmetic (typeof_ssize_t ()) : Abstract)
external typeof_suseconds_t : unit -> arithmetic = "ctypes_typeof_suseconds_t"
module Suseconds = (val mkArithmetic (typeof_suseconds_t ()) : Abstract)
external typeof_time_t : unit -> arithmetic = "ctypes_typeof_time_t"
module Time = (val mkArithmetic (typeof_time_t ()) : Abstract)
external typeof_uid_t : unit -> arithmetic = "ctypes_typeof_uid_t"
module Uid = (val mkArithmetic (typeof_uid_t ()) : Abstract)
external typeof_useconds_t : unit -> arithmetic = "ctypes_typeof_useconds_t"
module Useconds = (val mkArithmetic (typeof_useconds_t ()) : Abstract)

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

(* Non-arithmetic types *)

external sizeof_key_t : unit -> int = "ctypes_sizeof_key_t"
module Key = (val mkAbstractSized ~size:(sizeof_key_t ()) : Abstract)
external sizeof_pthread_attr_t : unit -> int = "ctypes_sizeof_pthread_attr_t"
module Pthread_Attr = (val mkAbstractSized ~size:(sizeof_pthread_attr_t ()) : Abstract)
external sizeof_pthread_cond_t : unit -> int = "ctypes_sizeof_pthread_cond_t"
module Pthread_Cond = (val mkAbstractSized ~size:(sizeof_pthread_cond_t ()) : Abstract)
external sizeof_pthread_condattr_t : unit -> int = "ctypes_sizeof_pthread_condattr_t"
module Pthread_Condattr = (val mkAbstractSized ~size:(sizeof_pthread_condattr_t ()) : Abstract)
external sizeof_pthread_key_t : unit -> int = "ctypes_sizeof_pthread_key_t"
module Pthread_Key = (val mkAbstractSized ~size:(sizeof_pthread_key_t ()) : Abstract)
external sizeof_pthread_mutex_t : unit -> int = "ctypes_sizeof_pthread_mutex_t"
module Pthread_Mutex = (val mkAbstractSized ~size:(sizeof_pthread_mutex_t ()) : Abstract)
external sizeof_pthread_mutexattr_t : unit -> int = "ctypes_sizeof_pthread_mutexattr_t"
module Pthread_Mutexattr = (val mkAbstractSized ~size:(sizeof_pthread_mutexattr_t ()) : Abstract)
external sizeof_pthread_once_t : unit -> int = "ctypes_sizeof_pthread_once_t"
module Pthread_Once = (val mkAbstractSized ~size:(sizeof_pthread_once_t ()) : Abstract)
external sizeof_pthread_rwlock_t : unit -> int = "ctypes_sizeof_pthread_rwlock_t"
module Pthread_Rwlock = (val mkAbstractSized ~size:(sizeof_pthread_rwlock_t ()) : Abstract)
external sizeof_pthread_rwlockattr_t : unit -> int = "ctypes_sizeof_pthread_rwlockattr_t"
module Pthread_Rwlockattr = (val mkAbstractSized ~size:(sizeof_pthread_rwlockattr_t ()) : Abstract)
external sizeof_timer_t : unit -> int = "ctypes_sizeof_timer_t"
module Timer = (val mkAbstractSized ~size:(sizeof_timer_t ()) : Abstract)

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
