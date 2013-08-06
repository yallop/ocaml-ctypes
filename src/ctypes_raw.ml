(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Low-level unsafe interface for calling functions using libffi. *)

module Types :
sig
  open Signed
  open Unsigned

  type 'a ctype
  external sizeof : _ ctype -> int = "ctypes_sizeof"
  external alignment : _ ctype -> int = "ctypes_alignment"
  external ctype_name : _ ctype -> string = "ctypes_typename"
  external passable : _ ctype -> bool = "ctypes_passable"

  module PtrType : Signed.S
  type voidp = PtrType.t
  val null : voidp

  val pointer : voidp ctype

  val void : unit ctype
  val char : char ctype
  val schar : int ctype
  val uchar : uchar ctype
  val float : float ctype
  val double : float ctype
  val short : int ctype
  val int : int ctype
  val camlint : int ctype
  val long : long ctype
  val llong : llong ctype
  val nativeint : nativeint ctype
  val int8_t : int ctype
  val int16_t : int ctype
  val int32_t : int32 ctype
  val int64_t : int64 ctype
  val uint8_t : uint8 ctype
  val uint16_t : uint16 ctype
  val uint32_t : uint32 ctype
  val uint64_t : uint64 ctype
  val size_t : size_t ctype
  val ushort : ushort ctype
  val uint : uint ctype
  val ulong : ulong ctype
  val ullong : ullong ctype
  val complex32 : Complex.t ctype
  val complex64 : Complex.t ctype
end =
struct
  open Unsigned
  open Signed

  type 'a ctype

  external sizeof : _ ctype -> int = "ctypes_sizeof"

  external alignment : _ ctype -> int = "ctypes_alignment"

  external ctype_name : _ ctype -> string = "ctypes_typename"

  external passable : _ ctype -> bool = "ctypes_passable"

  external _int_type_info : unit -> int ctype = "ctypes_int_type_info"
  let int = _int_type_info ()

  external _camlint_type_info : unit -> int ctype = "ctypes_camlint_type_info"
  let camlint = _camlint_type_info ()

  external _long_type_info : unit -> long ctype = "ctypes_long_type_info"
  let long = _long_type_info ()

  external _llong_type_info : unit -> llong ctype = "ctypes_llong_type_info"
  let llong = _llong_type_info ()

  external _nativeint_type_info : unit -> nativeint ctype = "ctypes_nativeint_type_info"
  let nativeint = _nativeint_type_info ()

  external _double_type_info : unit -> float ctype = "ctypes_double_type_info"
  let double = _double_type_info ()

  external _voidp_type_info : unit -> _ ctype = "ctypes_voidp_type_info"

  module PtrType = (val match sizeof (_voidp_type_info ()) with
      4 -> (module Signed.Int32 : Signed.S)
    | 8 -> (module Signed.Int64 : Signed.S)
    | _ -> failwith "No suitable type available to represent pointers.")
  type voidp = PtrType.t
  let pointer = _voidp_type_info ()

  external _void_type_info : unit -> unit ctype = "ctypes_void_type_info"
  let void = _void_type_info ()

  external _int8_t_type_info : unit -> int ctype = "ctypes_int8_t_type_info"
  let int8_t = _int8_t_type_info ()

  external _int16_t_type_info : unit -> int ctype = "ctypes_int16_t_type_info"
  let int16_t = _int16_t_type_info ()

  external _char_type_info : unit -> char ctype = "ctypes_char_type_info"
  let char = _char_type_info ()

  external _schar_type_info : unit -> int ctype = "ctypes_schar_type_info"
  let schar = _schar_type_info ()

  external _short_type_info : unit -> int ctype = "ctypes_short_type_info"
  let short = _short_type_info ()

  external _int32_t_type_info : unit -> int32 ctype = "ctypes_int32_t_type_info"
  let int32_t = _int32_t_type_info ()

  external _int64_t_type_info : unit -> int64 ctype = "ctypes_int64_t_type_info"
  let int64_t = _int64_t_type_info ()

  external _float_type_info : unit -> float ctype = "ctypes_float_type_info"
  let float = _float_type_info ()

  external _uchar_type_info : unit -> uchar ctype = "ctypes_uchar_type_info"
  let uchar = _uchar_type_info ()

  external _uint8_t_type_info : unit -> uint8 ctype = "ctypes_uint8_t_type_info"
  let uint8_t = _uint8_t_type_info ()

  external _uint16_t_type_info : unit -> uint16 ctype = "ctypes_uint16_t_type_info"
  let uint16_t = _uint16_t_type_info ()

  external _uint32_t_type_info : unit -> uint32 ctype = "ctypes_uint32_t_type_info"
  let uint32_t = _uint32_t_type_info ()

  external _uint64_t_type_info : unit -> uint64 ctype = "ctypes_uint64_t_type_info"
  let uint64_t = _uint64_t_type_info ()

  external _size_t_type_info : unit -> size_t ctype = "ctypes_size_t_type_info"
  let size_t = _size_t_type_info ()

  external _ushort_type_info : unit -> ushort ctype = "ctypes_ushort_type_info"
  let ushort = _ushort_type_info ()

  external _uint_type_info : unit -> uint ctype = "ctypes_uint_type_info"
  let uint = _uint_type_info ()

  external _ulong_type_info : unit -> ulong ctype = "ctypes_ulong_type_info"
  let ulong = _ulong_type_info ()

  external _ullong_type_info : unit -> ullong ctype = "ctypes_ullong_type_info"
  let ullong = _ullong_type_info ()

  external _complex32_type_info : unit -> Complex.t ctype = "ctypes_float_complex_type_info"
  let complex32 = _complex32_type_info ()

  external _complex64_type_info : unit -> Complex.t ctype = "ctypes_double_complex_type_info"
  let complex64 = _complex64_type_info ()

  external _null : unit -> voidp = "ctypes_null_value"
  let null = _null ()
end

open Types

(* A specification of argument C-types and C-return values *)
type bufferspec

(* A raw pointer to a block of memory, not managed by the garbage collector,
   into which we can read/write C values. *)
type raw_pointer = voidp

(* Read a C value from a block of memory *)
external read : 'a ctype -> offset:int -> raw_pointer -> 'a
  = "ctypes_read"

(* Write a C value to a block of memory *)
external write :  'a ctype -> offset:int -> 'a -> raw_pointer -> unit
  = "ctypes_write"

(* Return a string representation of a C value *)
external string_of : 'a ctype -> 'a -> string = "ctypes_string_of"

(* Allocate a new C call specification *)
external allocate_callspec : unit -> bufferspec
  = "ctypes_allocate_callspec"

(* Pass the return type and conclude the specification preparation *)
external prep_callspec : bufferspec -> _ ctype -> unit
  = "ctypes_prep_callspec"

(* Call the function specified by `bufferspec' at the given address.
   The callback functions write the arguments to the buffer and read
   the return value. *)
external call : raw_pointer -> bufferspec -> (raw_pointer -> unit) ->
  (raw_pointer -> 'a) -> 'a
  = "ctypes_call"

(* As ctypes_call, but check errno and raise Unix_error if the call failed. *)
external call_errno : string -> raw_pointer -> bufferspec ->
  (raw_pointer -> unit) -> (raw_pointer -> 'a) -> 'a
  = "ctypes_call_errno"

type managed_buffer
type _ structure = managed_buffer

(* Allocate a new C typed buffer specification *)
external allocate_bufferspec : unit -> bufferspec
  = "ctypes_allocate_bufferspec"

(* Produce a structure type representation from the buffer specification. *)
external complete_struct_type : bufferspec -> _ structure ctype
  = "ctypes_complete_structspec"

(* Add an argument to the C buffer specification *)
external add_argument : bufferspec -> _ ctype -> int
  = "ctypes_add_argument"

(* add_unpassable_argument : bufferspec -> size:int -> alignment:int -> int *)
external add_unpassable_argument : bufferspec -> size:int -> alignment:int -> int
  = "ctypes_add_unpassable_argument"

(* nary callbacks *)
type boxedfn =
  | Done of (raw_pointer -> unit) * bufferspec
  | Fn of (raw_pointer -> boxedfn)

(* Construct a pointer to an OCaml function represented by an identifier *)
external make_function_pointer : bufferspec -> int -> raw_pointer
  = "ctypes_make_function_pointer"

(* Allocate a region of stable memory managed by a custom block. *)
external allocate : int -> managed_buffer
  = "ctypes_allocate"

(* Obtain the address of the managed block. *)
external block_address : managed_buffer -> raw_pointer
  = "ctypes_block_address"

(* Set the function used to retrieve functions by identifier. *)
external set_closure_callback : (int -> boxedfn) -> unit
  = "ctypes_set_closure_callback"

(* Copy [size] bytes from [src + src_offset] to [dst + dst_offset]. *)
external memcpy :
  dst:raw_pointer -> dst_offset:int ->
  src:raw_pointer -> src_offset:int ->
    size:int -> unit
  = "ctypes_memcpy"

(* Convert a C string to an OCaml string *)
external string_of_cstring : raw_pointer -> int -> string
  = "ctypes_string_of_cstring"

(* Convert an OCaml string to a C string *)
external cstring_of_string : string -> managed_buffer
  = "ctypes_cstring_of_string"

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
