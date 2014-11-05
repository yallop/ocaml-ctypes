(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

module type BINDINGS = functor (F : Ctypes_types.TYPE) -> sig end

val write_c : Format.formatter -> (module BINDINGS) -> unit
