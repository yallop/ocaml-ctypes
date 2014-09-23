(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Representation of primitive C types.

   Internal representation, not for public use. *)

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

type _ ml_prim = 
  | ML_char :  char ml_prim
  | ML_float :  float ml_prim
  | ML_int :  int ml_prim
  | ML_int32 :  int32 ml_prim
  | ML_int64 :  int64 ml_prim
  | ML_nativeint :  nativeint ml_prim

val ml_prim : 'a prim -> 'a ml_prim
