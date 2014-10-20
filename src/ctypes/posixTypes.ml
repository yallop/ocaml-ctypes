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
external typeof_clock_t : unit -> arithmetic = "ctypes_typeof_clock_t"
external typeof_dev_t : unit -> arithmetic = "ctypes_typeof_dev_t"
external typeof_ino_t : unit -> arithmetic = "ctypes_typeof_ino_t"
external typeof_mode_t : unit -> arithmetic = "ctypes_typeof_mode_t"
external typeof_nlink_t : unit -> arithmetic = "ctypes_typeof_nlink_t"
external typeof_off_t : unit -> arithmetic = "ctypes_typeof_off_t"
external typeof_pid_t : unit -> arithmetic = "ctypes_typeof_pid_t"
external typeof_ssize_t : unit -> arithmetic = "ctypes_typeof_ssize_t"
external typeof_time_t : unit -> arithmetic = "ctypes_typeof_time_t"
external typeof_useconds_t : unit -> arithmetic = "ctypes_typeof_useconds_t"

module Clock = (val mkArithmetic (typeof_clock_t ()) : Abstract)
module Dev = (val mkArithmetic (typeof_dev_t ()) : Abstract)
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
module Time = (val mkArithmetic (typeof_time_t ()) : Abstract)
module Useconds = (val mkArithmetic (typeof_useconds_t ()) : Abstract)

type clock_t = Clock.t
type dev_t = Dev.t
type ino_t = Ino.t
type mode_t = Mode.t
type nlink_t = Nlink.t
type off_t = Off.t
type pid_t = Pid.t
type size_t = Size.t
type ssize_t = Ssize.t
type time_t = Time.t
type useconds_t = Useconds.t

let clock_t = Clock.t
let dev_t = Dev.t
let ino_t = Ino.t
let mode_t = Mode.t
let nlink_t = Nlink.t
let off_t = Off.t
let pid_t = Pid.t
let size_t = Size.t
let ssize_t = Ssize.t
let time_t = Time.t
let useconds_t = Useconds.t

(* Non-arithmetic types *)

external sizeof_sigset_t : unit -> int = "ctypes_sizeof_sigset_t"

external alignmentof_sigset_t : unit -> int = "ctypes_alignmentof_sigset_t"

module Sigset = (val mkAbstractSized ~name:"sigset_t" ~size:(sizeof_sigset_t ()) ~alignment:(alignmentof_sigset_t ()) : Abstract)

type sigset_t = Sigset.t

let sigset_t = Sigset.t
