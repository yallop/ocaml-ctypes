(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Ctypes

module type STRUCT =
sig
  type _ typ
  type (_, _) field

  val structure : string -> 's structure typ
  val union : string -> 's union typ

  val field : 't typ -> string -> 'a Ctypes.typ ->
    ('a, (('s, [<`Struct | `Union]) structured as 't)) field

  val seal : (_, [< `Struct | `Union]) structured typ -> unit
end

module type BINDINGS = functor (F : STRUCT) -> sig end

val write_c : Format.formatter -> (module BINDINGS) -> unit
