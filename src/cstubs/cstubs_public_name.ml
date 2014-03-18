(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Publicly visible names for type values *)

open Static
open Ctypes_path

let ident_of_ml_prim : type a. a Primitives.ml_prim -> path =
  let open Primitives in function
   | ML_char -> path_of_string "char"
   | ML_complex -> path_of_string "Complex.t"
   | ML_float -> path_of_string "float"
   | ML_int -> path_of_string "int"
   | ML_int32 -> path_of_string "int32"
   | ML_int64 -> path_of_string "int64"
   | ML_llong -> path_of_string "Signed.llong"
   | ML_long -> path_of_string "Signed.long"
   | ML_nativeint -> path_of_string "nativeint"
   | ML_size_t -> path_of_string "Unsigned.size_t"
   | ML_uchar -> path_of_string "Unsigned.uchar"
   | ML_uint -> path_of_string "Unsigned.uint"
   | ML_uint16 -> path_of_string "Unsigned.uint16"
   | ML_uint32 -> path_of_string "Unsigned.uint32"
   | ML_uint64 -> path_of_string "Unsigned.uint64"
   | ML_uint8 -> path_of_string "Unsigned.uint8"
   | ML_ullong -> path_of_string "Unsigned.ullong"
   | ML_ulong -> path_of_string "Unsigned.ulong"
   | ML_ushort -> path_of_string "Unsigned.ushort"

let constructor_ident_of_prim : type a. a Primitives.prim -> path =
  let open Primitives in function
   | Char -> path_of_string "Ctypes.char"
   | Schar -> path_of_string "Ctypes.schar"
   | Uchar -> path_of_string "Ctypes.uchar"
   | Short -> path_of_string "Ctypes.short"
   | Int -> path_of_string "Ctypes.int"
   | Long -> path_of_string "Ctypes.long"
   | Llong -> path_of_string "Ctypes.llong"
   | Ushort -> path_of_string "Ctypes.ushort"
   | Uint -> path_of_string "Ctypes.uint"
   | Ulong -> path_of_string "Ctypes.ulong"
   | Ullong -> path_of_string "Ctypes.ullong"
   | Size_t -> path_of_string "Ctypes.size_t"
   | Int8_t -> path_of_string "Ctypes.int8_t"
   | Int16_t -> path_of_string "Ctypes.int16_t"
   | Int32_t -> path_of_string "Ctypes.int32_t"
   | Int64_t -> path_of_string "Ctypes.int64_t"
   | Uint8_t -> path_of_string "Ctypes.uint8_t"
   | Uint16_t -> path_of_string "Ctypes.uint16_t"
   | Uint32_t -> path_of_string "Ctypes.uint32_t"
   | Uint64_t -> path_of_string "Ctypes.uint64_t"
   | Camlint -> path_of_string "Ctypes.camlint"
   | Nativeint -> path_of_string "Ctypes.nativeint"
   | Float -> path_of_string "Ctypes.float"
   | Double -> path_of_string "Ctypes.double"
   | Complex32 -> path_of_string "Ctypes.complex32"
   | Complex64 -> path_of_string "Ctypes.complex64"

let constructor_cident_of_prim : type a. a Primitives.prim -> path =
  let open Primitives in function
   | Char -> path_of_string "Cstubs_internals.Char"
   | Schar -> path_of_string "Cstubs_internals.Schar"
   | Uchar -> path_of_string "Cstubs_internals.Uchar"
   | Short -> path_of_string "Cstubs_internals.Short"
   | Int -> path_of_string "Cstubs_internals.Int"
   | Long -> path_of_string "Cstubs_internals.Long"
   | Llong -> path_of_string "Cstubs_internals.Llong"
   | Ushort -> path_of_string "Cstubs_internals.Ushort"
   | Uint -> path_of_string "Cstubs_internals.Uint"
   | Ulong -> path_of_string "Cstubs_internals.Ulong"
   | Ullong -> path_of_string "Cstubs_internals.Ullong"
   | Size_t -> path_of_string "Cstubs_internals.Size_t"
   | Int8_t -> path_of_string "Cstubs_internals.Int8_t"
   | Int16_t -> path_of_string "Cstubs_internals.Int16_t"
   | Int32_t -> path_of_string "Cstubs_internals.Int32_t"
   | Int64_t -> path_of_string "Cstubs_internals.Int64_t"
   | Uint8_t -> path_of_string "Cstubs_internals.Uint8_t"
   | Uint16_t -> path_of_string "Cstubs_internals.Uint16_t"
   | Uint32_t -> path_of_string "Cstubs_internals.Uint32_t"
   | Uint64_t -> path_of_string "Cstubs_internals.Uint64_t"
   | Camlint -> path_of_string "Cstubs_internals.Camlint"
   | Nativeint -> path_of_string "Cstubs_internals.Nativeint"
   | Float -> path_of_string "Cstubs_internals.Float"
   | Double -> path_of_string "Cstubs_internals.Double"
   | Complex32 -> path_of_string "Cstubs_internals.Complex32"
   | Complex64 -> path_of_string "Cstubs_internals.Complex64"
