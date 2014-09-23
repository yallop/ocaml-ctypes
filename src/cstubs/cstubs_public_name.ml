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
   | ML_float -> path_of_string "float"
   | ML_int -> path_of_string "int"
   | ML_int32 -> path_of_string "int32"
   | ML_int64 -> path_of_string "int64"
   | ML_nativeint -> path_of_string "nativeint"

let constructor_ident_of_prim : type a. a Primitives.prim -> path =
  let open Primitives in function
   | Char -> path_of_string "Ctypes.char"
   | Schar -> path_of_string "Ctypes.schar"
   | Short -> path_of_string "Ctypes.short"
   | Int -> path_of_string "Ctypes.int"
   | Int8_t -> path_of_string "Ctypes.int8_t"
   | Int16_t -> path_of_string "Ctypes.int16_t"
   | Int32_t -> path_of_string "Ctypes.int32_t"
   | Int64_t -> path_of_string "Ctypes.int64_t"
   | Camlint -> path_of_string "Ctypes.camlint"
   | Nativeint -> path_of_string "Ctypes.nativeint"
   | Float -> path_of_string "Ctypes.float"
   | Double -> path_of_string "Ctypes.double"

let constructor_cident_of_prim :
  type a. ?module_name:string -> a Primitives.prim -> path =
  fun ?(module_name="Cstubs_internals") ->
    let path ident =
      path_of_string (Printf.sprintf "%s.%s" module_name ident)
    in Primitives.(function
    | Char -> path "Char"
    | Schar -> path "Schar"
    | Short -> path "Short"
    | Int -> path "Int"
    | Int8_t -> path "Int8_t"
    | Int16_t -> path "Int16_t"
    | Int32_t -> path "Int32_t"
    | Int64_t -> path "Int64_t"
    | Camlint -> path "Camlint"
    | Nativeint -> path "Nativeint"
    | Float -> path "Float"
    | Double -> path "Double")
