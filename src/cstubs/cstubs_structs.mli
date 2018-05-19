(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

module type TYPE = Ctypes.TYPE
  [@@deprecated "Cstubs_structs.TYPE is deprecated. Use Ctypes.TYPE instead"]

module type BINDINGS = functor (F : Ctypes.TYPE) -> sig end

val write_c : Format.formatter -> (module BINDINGS) -> unit
