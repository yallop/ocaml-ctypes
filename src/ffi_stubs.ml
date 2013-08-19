(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Stubs for binding to libffi. *)

open Ctypes_raw

(* The type of structure types *)
type 'a ffitype (* a pointer *)
type struct_ffitype

external primitive_ffitype : 'a Primitives.prim -> 'a ffitype
 = "ctypes_primitive_ffitype"

external pointer_ffitype : unit -> voidp ffitype
 = "ctypes_pointer_ffitype"

external void_ffitype : unit -> unit ffitype
 = "ctypes_void_ffitype"


(* Allocate a new C typed buffer specification *)
external allocate_struct_ffitype : int -> struct_ffitype
  = "ctypes_allocate_struct_ffitype"

external struct_type_set_argument : struct_ffitype -> int -> _ ffitype -> unit
  = "ctypes_struct_ffitype_set_argument"

(* Produce a structure type representation from the buffer specification. *)
external complete_struct_type : struct_ffitype -> unit
  = "ctypes_complete_structspec"

external ffi_type_of_struct_type : struct_ffitype -> _ ffitype
  = "ctypes_block_address"

(* A specification of argument C-types and C-return values *)
type callspec

(* Allocate a new C call specification *)
external allocate_callspec : unit -> callspec
  = "ctypes_allocate_callspec"

(* Add an argument to the C buffer specification *)
external add_argument : callspec -> _ ffitype -> int
  = "ctypes_add_argument"

(* Pass the return type and conclude the specification preparation *)
external prep_callspec : callspec -> _ ffitype -> unit
  = "ctypes_prep_callspec"

(* Call the function specified by `callspec' at the given address.
   The callback functions write the arguments to the buffer and read
   the return value. *)
external call : voidp -> callspec -> (voidp -> unit) ->
  (voidp -> 'a) -> 'a
  = "ctypes_call"

(* As ctypes_call, but check errno and raise Unix_error if the call failed. *)
external call_errno : string -> voidp -> callspec ->
  (voidp -> unit) -> (voidp -> 'a) -> 'a
  = "ctypes_call_errno"


(* nary callbacks *)
type boxedfn =
  | Done of (voidp -> unit) * callspec
  | Fn of (voidp -> boxedfn)

(* Construct a pointer to an OCaml function represented by an identifier *)
external make_function_pointer : callspec -> int -> voidp
  = "ctypes_make_function_pointer"

(* Set the function used to retrieve functions by identifier. *)
external set_closure_callback : (int -> Obj.t) -> unit
  = "ctypes_set_closure_callback"


(* An internal error: for example, an `ffi_type' object passed to ffi_prep_cif
   was incorrect. *)
exception Ffi_internal_error of string
let () = Callback.register_exception "FFI_internal_error"
  (Ffi_internal_error "")

(* A closure passed to C was collected by the OCaml garbage collector before
   it was called. *)
exception CallToExpiredClosure
let () = Callback.register_exception "CallToExpiredClosure"
  CallToExpiredClosure
