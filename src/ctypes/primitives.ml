(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

type _ prim =
 | Char : char prim
 | Schar : int prim
 | Short : int prim
 | Int : int prim
 | Int8_t : int prim
 | Int16_t : int prim
 | Int32_t : int32 prim
 | Int64_t : int64 prim
 | Camlint : int prim
 | Nativeint : nativeint prim
 | Float : float prim
 | Double : float prim
 | Complex32 : Complex.t prim
 | Complex64 : Complex.t prim

type _ ml_prim = 
  | ML_char :  char ml_prim
  | ML_complex :  Complex.t ml_prim
  | ML_float :  float ml_prim
  | ML_int :  int ml_prim
  | ML_int32 :  int32 ml_prim
  | ML_int64 :  int64 ml_prim
  | ML_nativeint :  nativeint ml_prim

let ml_prim : type a. a prim -> a ml_prim = function
  | Char -> ML_char
  | Schar -> ML_int
  | Short -> ML_int
  | Int -> ML_int
  | Int8_t -> ML_int
  | Int16_t -> ML_int
  | Int32_t -> ML_int32
  | Int64_t -> ML_int64
  | Camlint -> ML_int
  | Nativeint -> ML_nativeint
  | Float -> ML_float
  | Double -> ML_float
  | Complex32 -> ML_complex
  | Complex64 -> ML_complex
