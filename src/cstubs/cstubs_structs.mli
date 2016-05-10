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

  val enum : string -> ?unexpected:(int64 -> 'a) -> ('a * int64 const) list -> 'a typ
end

module type BINDINGS = functor (F : TYPE) -> sig end

val write_c : Format.formatter -> (module BINDINGS) -> unit

module Easy :
sig
  val write_c : Format.formatter -> prefix:string -> (module BINDINGS) -> unit
  val write_ml : Format.formatter -> prefix:string -> (module BINDINGS) -> unit
end
