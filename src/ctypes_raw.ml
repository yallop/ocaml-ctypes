(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Interface to primitive types defined in C, and some exceptions. *)

module Types :
sig
  open Signed
  open Unsigned

  type 'a ctype_io
  type 'a ctype = {
    raw: 'a ctype_io;
    passable: bool;
  }

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

  type 'a ctype_io

  type 'a ctype = {
    raw: 'a ctype_io;
    passable: bool;
  }

  let initialize ?(passable=true) raw = { raw; passable }

  external _int_type_info : unit -> int ctype_io = "ctypes_int_type_info"
  let int = initialize (_int_type_info ())

  external _camlint_type_info : unit -> int ctype_io = "ctypes_camlint_type_info"
  let camlint = initialize (_camlint_type_info ())

  external _long_type_info : unit -> long ctype_io = "ctypes_long_type_info"
  let long = initialize (_long_type_info ())

  external _llong_type_info : unit -> llong ctype_io = "ctypes_llong_type_info"
  let llong = initialize (_llong_type_info ())

  external _nativeint_type_info : unit -> nativeint ctype_io = "ctypes_nativeint_type_info"
  let nativeint = initialize (_nativeint_type_info ())

  external _double_type_info : unit -> float ctype_io = "ctypes_double_type_info"
  let double = initialize (_double_type_info ())

  external _voidp_type_info : unit -> _ ctype_io = "ctypes_voidp_type_info"

  module PtrType = (val match Primitive_details.pointer_size with
      4 -> (module Signed.Int32 : Signed.S)
    | 8 -> (module Signed.Int64 : Signed.S)
    | _ -> failwith "No suitable type available to represent pointers.")
  type voidp = PtrType.t
  let pointer = initialize (_voidp_type_info ())

  external _void_type_info : unit -> unit ctype_io = "ctypes_void_type_info"
  let void = initialize ~passable:false (_void_type_info ())

  external _int8_t_type_info : unit -> int ctype_io = "ctypes_int8_t_type_info"
  let int8_t = initialize (_int8_t_type_info ())

  external _int16_t_type_info : unit -> int ctype_io = "ctypes_int16_t_type_info"
  let int16_t = initialize (_int16_t_type_info ())

  external _char_type_info : unit -> char ctype_io = "ctypes_char_type_info"
  let char = initialize (_char_type_info ())

  external _schar_type_info : unit -> int ctype_io = "ctypes_schar_type_info"
  let schar = initialize (_schar_type_info ())

  external _short_type_info : unit -> int ctype_io = "ctypes_short_type_info"
  let short = initialize (_short_type_info ())

  external _int32_t_type_info : unit -> int32 ctype_io = "ctypes_int32_t_type_info"
  let int32_t = initialize (_int32_t_type_info ())

  external _int64_t_type_info : unit -> int64 ctype_io = "ctypes_int64_t_type_info"
  let int64_t = initialize (_int64_t_type_info ())

  external _float_type_info : unit -> float ctype_io = "ctypes_float_type_info"
  let float = initialize (_float_type_info ())

  external _uchar_type_info : unit -> uchar ctype_io = "ctypes_uchar_type_info"
  let uchar = initialize (_uchar_type_info ())

  external _uint8_t_type_info : unit -> uint8 ctype_io = "ctypes_uint8_t_type_info"
  let uint8_t = initialize (_uint8_t_type_info ())

  external _uint16_t_type_info : unit -> uint16 ctype_io = "ctypes_uint16_t_type_info"
  let uint16_t = initialize (_uint16_t_type_info ())

  external _uint32_t_type_info : unit -> uint32 ctype_io = "ctypes_uint32_t_type_info"
  let uint32_t = initialize (_uint32_t_type_info ())

  external _uint64_t_type_info : unit -> uint64 ctype_io = "ctypes_uint64_t_type_info"
  let uint64_t = initialize (_uint64_t_type_info ())

  external _size_t_type_info : unit -> size_t ctype_io = "ctypes_size_t_type_info"
  let size_t = initialize (_size_t_type_info ())

  external _ushort_type_info : unit -> ushort ctype_io = "ctypes_ushort_type_info"
  let ushort = initialize (_ushort_type_info ())

  external _uint_type_info : unit -> uint ctype_io = "ctypes_uint_type_info"
  let uint = initialize (_uint_type_info ())

  external _ulong_type_info : unit -> ulong ctype_io = "ctypes_ulong_type_info"
  let ulong = initialize (_ulong_type_info ())

  external _ullong_type_info : unit -> ullong ctype_io = "ctypes_ullong_type_info"
  let ullong = initialize (_ullong_type_info ())

  external _complex32_type_info : unit -> Complex.t ctype_io = "ctypes_float_complex_type_info"
  let complex32 = initialize ~passable:false (_complex32_type_info ())

  external _complex64_type_info : unit -> Complex.t ctype_io = "ctypes_double_complex_type_info"
  let complex64 = initialize ~passable:false (_complex64_type_info ())

  external _null : unit -> voidp = "ctypes_null_value"
  let null = _null ()
end

open Types

(* A raw pointer to a block of memory, not managed by the garbage collector,
   into which we can read/write C values. *)
type raw_pointer = voidp

type managed_buffer

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

let ctype_of_prim : type a. a Primitives.prim -> a ctype = Primitives.(function
 | Char -> char
 | Schar -> schar
 | Uchar -> uchar
 | Short -> short
 | Int -> int
 | Long -> long
 | Llong -> llong
 | Ushort -> ushort
 | Uint -> uint
 | Ulong -> ulong
 | Ullong -> ullong
 | Size_t -> size_t
 | Int8_t -> int8_t
 | Int16_t -> int16_t
 | Int32_t -> int32_t
 | Int64_t -> int64_t
 | Uint8_t -> uint8_t
 | Uint16_t -> uint16_t
 | Uint32_t -> uint32_t
 | Uint64_t -> uint64_t
 | Camlint -> camlint
 | Nativeint -> nativeint
 | Float -> float
 | Double -> double
 | Complex32 -> complex32
 | Complex64 -> complex64
)
