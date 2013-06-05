(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

module type S = sig
  include Unsigned.S

  val neg : t -> t
  val abs : t -> t
  val minus_one : t
  val min_int : t
  val shift_right_logical : t -> int -> t
  val of_int64 : int64 -> t
  val to_int64 : t -> int64
end

module type Basics = sig
  type t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val rem : t -> t -> t
  val max_int : t
  val logand : t -> t -> t
  val logor : t -> t -> t
  val logxor : t -> t -> t
  val shift_left : t -> int -> t
  val shift_right : t -> int -> t
  val shift_right_logical : t -> int -> t
end

module MakeInfix(S : Basics) =
struct
  open S
  let (+) = add
  let (-) = sub
  let ( * ) = mul
  let (/) = div
  let (mod) = rem
  let (land) = logand
  let (lor) = logor
  let (lxor) = logxor
  let (lsl) = shift_left
  let (lsr) = shift_right_logical
  let (asr) = shift_right
end

module Int32 = 
struct
  include Int32
  module Infix = MakeInfix(Int32)
  let of_int64 = Int64.to_int32
  let to_int64 = Int64.of_int32
end

module Int64 = 
struct
  include Int64
  module Infix = MakeInfix(Int64)
  let of_int64 x = x
  let to_int64 x = x
end

(* C guarantees that sizeof(t) == sizeof(unsigned t) *)
external long_size : unit -> int = "ctypes_ulong_size"
external llong_size : unit -> int = "ctypes_ulonglong_size"

let pick : size:int -> (module S) =
  fun ~size -> match size with
    | 4 -> (module Int32)
    | 8 -> (module Int64)
    | _ -> assert false

module Long = (val pick ~size:(long_size ()))
module LLong = (val pick ~size:(llong_size ()))

type long = Long.t
type llong = LLong.t
