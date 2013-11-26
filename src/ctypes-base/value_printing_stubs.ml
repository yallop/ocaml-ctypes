(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Stubs for formatting C values. *)

(* Return a string representation of a C value *)
external string_of_prim : 'a Primitives.prim -> 'a -> string
  = "ctypes_string_of_prim"

external string_of_pointer : Ctypes_raw.voidp -> string
  = "ctypes_string_of_pointer"
