(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Stubs for standard views. *)

open Ctypes_raw

(* Convert a C string to an OCaml string *)
external string_of_cstring : raw_pointer -> int -> string
  = "ctypes_string_of_cstring"

(* Convert an OCaml string to a C string *)
external cstring_of_string : string -> managed_buffer
  = "ctypes_cstring_of_string"
