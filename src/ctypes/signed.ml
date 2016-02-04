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
  val of_nativeint : nativeint -> t
  val to_nativeint : t -> nativeint
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

(* Integers less than 32 bits *)
module IntSmall(M: sig val modulus : int end) : S =
struct
  module Conversions :
  sig
    type t = private int
    val max_int : t
    val min_int : t
    val of_int : int -> t
  end =
  struct
    type t = int
    let max_int = M.modulus
    let min_int = -(M.modulus + 1)
    let of_int x = ((x + min_int) mod max_int) - min_int
  end
  include Conversions
  let to_int (x : t) = (x :> int)

  let lift1 f x = of_int (f (x : t :> int))
  let lift2 f x y = of_int (f (x : t :> int) (y : t :> int))

  module Basics : Basics with type t = Conversions.t =
  struct
    type t = Conversions.t
    let add = lift2 ( + )
    let sub = lift2 ( - )
    let mul = lift2 ( * )
    let div = lift2 ( / )
    let rem = lift2 (mod)
    let max_int = max_int
    let logand = lift2 (land)
    let logor = lift2 (lor)
    let logxor = lift2 (lxor)
    let shift_left (l : t) r = of_int (to_int l lsl r)
    let shift_right (l : t) r = of_int (to_int l asr r)
    let shift_right_logical l r = of_int (to_int l lsr r)
  end
  include (Basics : Basics with type t := t)
  module Infix = MakeInfix(Basics)
  let to_int64 x = Int64.of_int (to_int x)
  let of_int64 x = of_int (Int64.to_int x)
  let to_nativeint x = (Nativeint.of_int (to_int x))
  let of_nativeint x = of_int (Nativeint.to_int x)
  let minus_one = of_int (-1)
  let abs = lift1 abs
  let neg = lift1 (fun x -> - x)
  let compare x y = Pervasives.compare (to_int x) (to_int y)
  let pred = lift1 pred
  let succ = lift1 succ
  let lognot = lift1 lnot
  let one = of_int 1
  let zero = of_int 0
  let to_string x = string_of_int (to_int x)
  let of_string x = of_int (int_of_string x)
end

module Int8 = IntSmall(struct let modulus = 255 end)
module Int16 = IntSmall(struct let modulus = 32767 end)

module Int32 = 
struct
  include Int32
  module Infix = MakeInfix(Int32)
  let of_nativeint = Nativeint.to_int32
  let to_nativeint = Nativeint.of_int32
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
