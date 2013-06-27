(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

module type Abstract =
sig
  type t
  val t : t Ctypes.typ
end

let mkAbstract : 'a. 'a Ctypes.typ -> (module Abstract)
  = fun (type a) (ty : a Ctypes.typ) ->
    (module
     struct
       type t = a
       let t = ty
     end : Abstract)

let mkAbstractSized : name:string -> size:int -> alignment:int -> (module Abstract)
  = fun ~name ~size ~alignment:a ->
    (module
     struct
       open Ctypes
       type t = unit Ctypes.abstract
       let t = abstract ~name ~size ~alignment:a
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
  let open Ctypes in function
    Int8   -> mkAbstract int8_t
  | Int16  -> mkAbstract int16_t
  | Int32  -> mkAbstract int32_t
  | Int64  -> mkAbstract int64_t
  | Uint8  -> mkAbstract uint8_t
  | Uint16 -> mkAbstract uint16_t
  | Uint32 -> mkAbstract uint32_t
  | Uint64 -> mkAbstract uint64_t
  | Float  -> mkAbstract float
  | Double -> mkAbstract double

(* Arithmetic types *)
external typeof_blkcnt_t : unit -> arithmetic = "ctypes_typeof_blkcnt_t"
external typeof_blksize_t : unit -> arithmetic = "ctypes_typeof_blksize_t"
external typeof_clock_t : unit -> arithmetic = "ctypes_typeof_clock_t"
external typeof_dev_t : unit -> arithmetic = "ctypes_typeof_dev_t"
external typeof_fsblkcnt_t : unit -> arithmetic = "ctypes_typeof_fsblkcnt_t"
external typeof_fsfilcnt_t : unit -> arithmetic = "ctypes_typeof_fsfilcnt_t"
external typeof_gid_t : unit -> arithmetic = "ctypes_typeof_gid_t"
external typeof_id_t : unit -> arithmetic = "ctypes_typeof_id_t"
external typeof_ino_t : unit -> arithmetic = "ctypes_typeof_ino_t"
external typeof_mode_t : unit -> arithmetic = "ctypes_typeof_mode_t"
external typeof_nlink_t : unit -> arithmetic = "ctypes_typeof_nlink_t"
external typeof_off_t : unit -> arithmetic = "ctypes_typeof_off_t"
external typeof_pid_t : unit -> arithmetic = "ctypes_typeof_pid_t"
external typeof_ssize_t : unit -> arithmetic = "ctypes_typeof_ssize_t"
external typeof_suseconds_t : unit -> arithmetic = "ctypes_typeof_suseconds_t"
external typeof_time_t : unit -> arithmetic = "ctypes_typeof_time_t"
external typeof_uid_t : unit -> arithmetic = "ctypes_typeof_uid_t"
external typeof_useconds_t : unit -> arithmetic = "ctypes_typeof_useconds_t"

module Blkcnt = (val mkArithmetic (typeof_blkcnt_t ()) : Abstract)
module Blksize = (val mkArithmetic (typeof_blksize_t ()) : Abstract)
module Clock = (val mkArithmetic (typeof_clock_t ()) : Abstract)
module Dev = (val mkArithmetic (typeof_dev_t ()) : Abstract)
module Fsblkcnt = (val mkArithmetic (typeof_fsblkcnt_t ()) : Abstract)
module Fsfilcnt = (val mkArithmetic (typeof_fsfilcnt_t ()) : Abstract)
module Gid = (val mkArithmetic (typeof_gid_t ()) : Abstract)
module Id = (val mkArithmetic (typeof_id_t ()) : Abstract)
module Ino = (val mkArithmetic (typeof_ino_t ()) : Abstract)
module Mode = (val mkArithmetic (typeof_mode_t ()) : Abstract)
module Nlink = (val mkArithmetic (typeof_nlink_t ()) : Abstract)
module Off = (val mkArithmetic (typeof_off_t ()) : Abstract)
module Pid = (val mkArithmetic (typeof_pid_t ()) : Abstract)
module Size = 
struct
  type t = Unsigned.size_t
  let t = Ctypes.size_t
end
module Ssize = (val mkArithmetic (typeof_ssize_t ()) : Abstract)
module Suseconds = (val mkArithmetic (typeof_suseconds_t ()) : Abstract)
module Time = (val mkArithmetic (typeof_time_t ()) : Abstract)
module Uid = (val mkArithmetic (typeof_uid_t ()) : Abstract)
module Useconds = (val mkArithmetic (typeof_useconds_t ()) : Abstract)

type blkcnt_t = Blkcnt.t
type blksize_t = Blksize.t
type clock_t = Clock.t
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
type size_t = Size.t
type ssize_t = Ssize.t
type suseconds_t = Suseconds.t
type time_t = Time.t
type uid_t = Uid.t
type useconds_t = Useconds.t

let blkcnt_t = Blkcnt.t
let blksize_t = Blksize.t
let clock_t = Clock.t
let dev_t = Dev.t
let fsblkcnt_t = Fsblkcnt.t
let fsfilcnt_t = Fsfilcnt.t
let gid_t = Gid.t
let id_t = Id.t
let ino_t = Ino.t
let mode_t = Mode.t
let nlink_t = Nlink.t
let off_t = Off.t
let pid_t = Pid.t
let size_t = Size.t
let ssize_t = Ssize.t
let suseconds_t = Suseconds.t
let time_t = Time.t
let uid_t = Uid.t
let useconds_t = Useconds.t

(* Non-arithmetic types *)

external sizeof_key_t : unit -> int = "ctypes_sizeof_key_t"
external sizeof_pthread_t : unit -> int = "ctypes_sizeof_pthread_t"
external sizeof_pthread_attr_t : unit -> int = "ctypes_sizeof_pthread_attr_t"
external sizeof_pthread_cond_t : unit -> int = "ctypes_sizeof_pthread_cond_t"
external sizeof_pthread_condattr_t : unit -> int = "ctypes_sizeof_pthread_condattr_t"
external sizeof_pthread_key_t : unit -> int = "ctypes_sizeof_pthread_key_t"
external sizeof_pthread_mutex_t : unit -> int = "ctypes_sizeof_pthread_mutex_t"
external sizeof_pthread_mutexattr_t : unit -> int = "ctypes_sizeof_pthread_mutexattr_t"
external sizeof_pthread_once_t : unit -> int = "ctypes_sizeof_pthread_once_t"
external sizeof_pthread_rwlock_t : unit -> int = "ctypes_sizeof_pthread_rwlock_t"
external sizeof_pthread_rwlockattr_t : unit -> int = "ctypes_sizeof_pthread_rwlockattr_t"
external sizeof_sigset_t : unit -> int = "ctypes_sizeof_sigset_t"

external alignmentof_key_t : unit -> int = "ctypes_alignmentof_key_t"
external alignmentof_pthread_t : unit -> int = "ctypes_alignmentof_pthread_t"
external alignmentof_pthread_attr_t : unit -> int = "ctypes_alignmentof_pthread_attr_t"
external alignmentof_pthread_cond_t : unit -> int = "ctypes_alignmentof_pthread_cond_t"
external alignmentof_pthread_condattr_t : unit -> int = "ctypes_alignmentof_pthread_condattr_t"
external alignmentof_pthread_key_t : unit -> int = "ctypes_alignmentof_pthread_key_t"
external alignmentof_pthread_mutex_t : unit -> int = "ctypes_alignmentof_pthread_mutex_t"
external alignmentof_pthread_mutexattr_t : unit -> int = "ctypes_alignmentof_pthread_mutexattr_t"
external alignmentof_pthread_once_t : unit -> int = "ctypes_alignmentof_pthread_once_t"
external alignmentof_pthread_rwlock_t : unit -> int = "ctypes_alignmentof_pthread_rwlock_t"
external alignmentof_pthread_rwlockattr_t : unit -> int = "ctypes_alignmentof_pthread_rwlockattr_t"
external alignmentof_sigset_t : unit -> int = "ctypes_alignmentof_sigset_t"

module Key = (val mkAbstractSized ~name:"key_t" ~size:(sizeof_key_t ()) ~alignment:(alignmentof_key_t ()) : Abstract)
module Pthread = (val mkAbstractSized ~name:"pthread_t" ~size:(sizeof_pthread_t ()) ~alignment:(alignmentof_pthread_t ()) : Abstract)
module Pthread_attr = (val mkAbstractSized ~name:"pthread_attr_t" ~size:(sizeof_pthread_attr_t ()) ~alignment:(alignmentof_pthread_attr_t ()) : Abstract)
module Pthread_cond = (val mkAbstractSized ~name:"pthread_cond_t" ~size:(sizeof_pthread_cond_t ()) ~alignment:(alignmentof_pthread_cond_t ()) : Abstract)
module Pthread_condattr = (val mkAbstractSized ~name:"pthread_condattr_t" ~size:(sizeof_pthread_condattr_t ()) ~alignment:(alignmentof_pthread_condattr_t ()) : Abstract)
module Pthread_key = (val mkAbstractSized ~name:"pthread_key_t" ~size:(sizeof_pthread_key_t ()) ~alignment:(alignmentof_pthread_key_t ()) : Abstract)
module Pthread_mutex = (val mkAbstractSized ~name:"pthread_mutex_t" ~size:(sizeof_pthread_mutex_t ()) ~alignment:(alignmentof_pthread_mutex_t ()) : Abstract)
module Pthread_mutexattr = (val mkAbstractSized ~name:"pthread_mutexattr_t" ~size:(sizeof_pthread_mutexattr_t ()) ~alignment:(alignmentof_pthread_mutexattr_t ()) : Abstract)
module Pthread_once = (val mkAbstractSized ~name:"pthread_once_t" ~size:(sizeof_pthread_once_t ()) ~alignment:(alignmentof_pthread_once_t ()) : Abstract)
module Pthread_rwlock = (val mkAbstractSized ~name:"pthread_rwlock_t" ~size:(sizeof_pthread_rwlock_t ()) ~alignment:(alignmentof_pthread_rwlock_t ()) : Abstract)
module Pthread_rwlockattr = (val mkAbstractSized ~name:"pthread_rwlockattr_t" ~size:(sizeof_pthread_rwlockattr_t ()) ~alignment:(alignmentof_pthread_rwlockattr_t ()) : Abstract)
module Sigset = (val mkAbstractSized ~name:"sigset_t" ~size:(sizeof_sigset_t ()) ~alignment:(alignmentof_sigset_t ()) : Abstract)

type key_t = Key.t
type pthread_t = Pthread.t
type pthread_attr_t = Pthread_attr.t
type pthread_cond_t = Pthread_cond.t
type pthread_condattr_t = Pthread_condattr.t
type pthread_key_t = Pthread_key.t
type pthread_mutex_t = Pthread_mutex.t
type pthread_mutexattr_t = Pthread_mutexattr.t
type pthread_once_t = Pthread_once.t
type pthread_rwlock_t = Pthread_rwlock.t
type pthread_rwlockattr_t = Pthread_rwlockattr.t
type sigset_t = Sigset.t

let key_t = Key.t
let pthread_t = Pthread.t
let pthread_attr_t = Pthread_attr.t
let pthread_cond_t = Pthread_cond.t
let pthread_condattr_t = Pthread_condattr.t
let pthread_key_t = Pthread_key.t
let pthread_mutex_t = Pthread_mutex.t
let pthread_mutexattr_t = Pthread_mutexattr.t
let pthread_once_t = Pthread_once.t
let pthread_rwlock_t = Pthread_rwlock.t
let pthread_rwlockattr_t = Pthread_rwlockattr.t
let sigset_t = Sigset.t
