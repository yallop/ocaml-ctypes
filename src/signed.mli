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
end

module Int32 : S with type t = int32
module Int64 : S with type t = int64
module Long : S
module LLong : S

type long = Long.t
type llong = LLong.t
