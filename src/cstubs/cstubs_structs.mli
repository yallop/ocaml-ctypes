(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

module type TYPE =
sig
  include Ctypes_types.TYPE

  type 'a const
  val constant : string -> 'a typ -> 'a const

  module type signed = sig include Signed.S val t : t typ end
  val signed  : string -> (module signed)

  module type unsigned = sig include Unsigned.S val t : t typ end
  val unsigned : string -> (module unsigned)

  val enum : string -> ?typedef:bool -> ?unexpected:(int64 -> 'a) -> ('a * int64 const) list -> 'a typ
end

module type BINDINGS = functor (F : TYPE) -> sig end

val write_c : Format.formatter -> (module BINDINGS) -> unit
