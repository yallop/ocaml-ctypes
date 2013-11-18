(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Coercions *)

open Static

exception Uncoercible

let id x = x

let coerce_ml_prim :
  type a b. a Primitives.ml_prim -> b Primitives.ml_prim -> a -> b =
  let open Primitives in 
  fun l r -> match l, r with
  | ML_char, ML_char -> id
  | ML_complex, ML_complex -> id
  | ML_float, ML_float -> id
  | ML_int, ML_int -> id
  | ML_int32, ML_int32 -> id
  | ML_int64, ML_int64 -> id
  | ML_llong, ML_llong -> id
  | ML_long, ML_long -> id
  | ML_nativeint, ML_nativeint -> id
  | ML_size_t, ML_size_t -> id
  | ML_uchar, ML_uchar -> id
  | ML_uint, ML_uint -> id
  | ML_uint16, ML_uint16 -> id
  | ML_uint32, ML_uint32 -> id
  | ML_uint64, ML_uint64 -> id
  | ML_uint8, ML_uint8 -> id
  | ML_ullong, ML_ullong -> id
  | ML_ulong, ML_ulong -> id
  | ML_ushort, ML_ushort -> id
  | _ -> raise Uncoercible

let rec coerce : type a b. a typ -> b typ -> a -> b =
  fun atyp btyp -> match atyp, btyp with
  | _, Void -> fun _ -> ()
  | Primitive l, Primitive r ->
    Primitives.(coerce_ml_prim (ml_prim l) (ml_prim r))
  | View av, b ->
    let coerce = coerce av.ty b in
    fun v -> coerce (av.write v)
  | a, View bv ->
    let coerce = coerce a bv.ty in
    fun v -> bv.read (coerce v)
  | Pointer _, Pointer b ->
    fun v -> Memory.(from_voidp b (to_voidp v))
  | _ -> raise Uncoercible 
