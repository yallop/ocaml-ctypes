(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Stubs for standard views. *)

open Ctypes_raw

(* A specification of argument C-types and C-return values *)
type callspec

(* Allocate a new C call specification *)
external allocate_callspec : unit -> callspec
  = "ctypes_allocate_callspec"

(* Add an argument to the C buffer specification *)
external add_argument : callspec -> _ Types.ctype_io -> int
  = "ctypes_add_argument"

(* Pass the return type and conclude the specification preparation *)
external prep_callspec : callspec -> _ Types.ctype_io -> unit
  = "ctypes_prep_callspec"

(* Call the function specified by `callspec' at the given address.
   The callback functions write the arguments to the buffer and read
   the return value. *)
external call : raw_pointer -> callspec -> (raw_pointer -> unit) ->
  (raw_pointer -> 'a) -> 'a
  = "ctypes_call"

(* As ctypes_call, but check errno and raise Unix_error if the call failed. *)
external call_errno : string -> raw_pointer -> callspec ->
  (raw_pointer -> unit) -> (raw_pointer -> 'a) -> 'a
  = "ctypes_call_errno"


(* nary callbacks *)
type boxedfn =
  | Done of (raw_pointer -> unit) * callspec
  | Fn of (raw_pointer -> boxedfn)

(* Construct a pointer to an OCaml function represented by an identifier *)
external make_function_pointer : callspec -> int -> raw_pointer
  = "ctypes_make_function_pointer"

(* Set the function used to retrieve functions by identifier. *)
external set_closure_callback : (int -> Obj.t) -> unit
  = "ctypes_set_closure_callback"


(* Convert a C string to an OCaml string *)
external string_of_cstring : raw_pointer -> int -> string
  = "ctypes_string_of_cstring"

(* Convert an OCaml string to a C string *)
external cstring_of_string : string -> managed_buffer
  = "ctypes_cstring_of_string"
