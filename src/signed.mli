(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(** Types and operations for signed integers. *)

module type S = sig
  include Unsigned.S

  val neg : t -> t
  (** Unary negation. *)

  val abs : t -> t
  (** Return the absolute value of its argument. *)

  val minus_one : t
  (** The value -1 *)

  val min_int : t
  (** The smallest representable integer. *)

  val shift_right_logical : t -> int -> t
  (** {!shift_right_logical} [x] [y] shifts [x] to the right by [y] bits.  See
      {!Int32.shift_right_logical}. *)

  val of_int64 : int64 -> t
  (** Convert the given int64 value to a signed integer. *)

  val to_int64 : t -> int64
  (** Convert the given signed integer to an int64 value. *)
end
(** Signed integer operations *)

module Int32 : S with type t = int32
(** Signed 32-bit integer type and operations. *)

module Int64 : S with type t = int64
(** Signed 64-bit integer type and operations. *)

module Long : S
(** The signed long integer type and operations. *)

module LLong : S
(** The signed long long integer type and operations. *)

type long = Long.t
(** The signed long integer type. *)

type llong = LLong.t
(** The signed long long integer type. *)
