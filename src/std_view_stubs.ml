(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Stubs for standard views. *)

(* Convert a C string to an OCaml string *)
external string_of_cstring : Ctypes_raw.voidp -> int -> string
  = "ctypes_string_of_cstring"

(* Convert an OCaml string to a C string *)
external cstring_of_string : string -> Memory_stubs.managed_buffer
  = "ctypes_cstring_of_string"
