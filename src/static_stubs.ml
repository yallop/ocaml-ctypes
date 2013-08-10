(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Stubs for the static semantics. *)

open Ctypes_raw

external sizeof : _ Types.ctype -> int = "ctypes_sizeof"

external alignment : _ Types.ctype -> int = "ctypes_alignment"

external ctype_name : _ Types.ctype -> string = "ctypes_typename"

external passable : _ Types.ctype -> bool = "ctypes_passable"

(* Allocate a new C typed buffer specification *)
external allocate_bufferspec : unit -> bufferspec
  = "ctypes_allocate_bufferspec"

(* Add an argument to the C buffer specification *)
external add_argument : bufferspec -> _ Types.ctype -> int
  = "ctypes_add_argument"

(* Add an argument that makes the buffer unpassable to the C buffer
   specification *)
external add_unpassable_argument : bufferspec -> size:int -> alignment:int -> int
  = "ctypes_add_unpassable_argument"

(* Produce a structure type representation from the buffer specification. *)
external complete_struct_type : bufferspec -> _ structure Types.ctype
  = "ctypes_complete_structspec"
