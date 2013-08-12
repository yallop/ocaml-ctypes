(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Stubs for the static semantics. *)

open Ctypes_raw

(* A struct layout specification *)
type bufferspec

(* The type of struct values *)
type _ structure = Ctypes_raw.managed_buffer

(* Allocate a new C typed buffer specification *)
external allocate_bufferspec : unit -> bufferspec
  = "ctypes_allocate_bufferspec"

(* Add an argument to the C buffer specification *)
external add_argument : bufferspec -> _ Types.ctype_io -> int
  = "ctypes_add_argument"

(* Produce a structure type representation from the buffer specification. *)
external complete_struct_type : bufferspec -> _ structure Types.ctype_io
  = "ctypes_complete_structspec"

external make_unpassable_structspec : size:int -> alignment:int ->
  _ structure Types.ctype_io
    = "ctypes_make_unpassable_structspec"
