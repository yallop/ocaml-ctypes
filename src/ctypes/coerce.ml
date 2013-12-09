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

type (_, _) coercion =
  | Id : ('a, 'a) coercion
  | Coercion : ('a -> 'b) -> ('a, 'b) coercion

let ml_prim_coercion :
  type a b. a Primitives.ml_prim -> b Primitives.ml_prim -> (a, b) coercion =
  let open Primitives in 
  fun l r -> match l, r with
  | ML_char, ML_char -> Id
  | ML_complex, ML_complex -> Id
  | ML_float, ML_float -> Id
  | ML_int, ML_int -> Id
  | ML_int32, ML_int32 -> Id
  | ML_int64, ML_int64 -> Id
  | ML_llong, ML_llong -> Id
  | ML_long, ML_long -> Id
  | ML_nativeint, ML_nativeint -> Id
  | ML_size_t, ML_size_t -> Id
  | ML_uchar, ML_uchar -> Id
  | ML_uint, ML_uint -> Id
  | ML_uint16, ML_uint16 -> Id
  | ML_uint32, ML_uint32 -> Id
  | ML_uint64, ML_uint64 -> Id
  | ML_uint8, ML_uint8 -> Id
  | ML_ullong, ML_ullong -> Id
  | ML_ulong, ML_ulong -> Id
  | ML_ushort, ML_ushort -> Id
  | _ -> raise Uncoercible

let rec coercion : type a b. a typ -> b typ -> (a, b) coercion =
  fun atyp btyp -> match atyp, btyp with
  | _, Void -> Coercion ignore
  | Primitive l, Primitive r ->
    Primitives.(ml_prim_coercion (ml_prim l) (ml_prim r)) 
  | View av, b ->
    begin match coercion av.ty b with
    | Id -> Coercion av.write
    | Coercion coerce -> Coercion (fun v -> coerce (av.write v))
    end
  | a, View bv ->
    begin match coercion a bv.ty with
    | Id -> Coercion bv.read
    | Coercion coerce -> Coercion (fun v -> bv.read (coerce v))
    end
  | Pointer a, Pointer b ->
    begin
      try
        begin match coercion a b with
        | Id -> Id
        | Coercion _ -> Coercion (fun p -> { p with reftype = b })
        end
      with Uncoercible ->
        Coercion (fun p -> { p with reftype = b })
    end
  | _ -> raise Uncoercible 

let rec fn_coercion : type a b. a fn -> b fn -> (a, b) coercion =
  fun afn bfn -> match afn, bfn with
  | Function (af, at), Function (bf, bt) ->
    begin match coercion bf af, fn_coercion at bt with
    | Id, Id -> Id
    | Id, Coercion h ->
      Coercion (fun g x -> h (g x))
    | Coercion f, Id ->
      Coercion (fun g x -> g (f x))
    | Coercion f, Coercion h ->
      Coercion (fun g x -> h (g (f x)))
    end
  | Returns at, Returns bt -> coercion at bt
  | _ -> raise Uncoercible

let coerce : type a b. a typ -> b typ -> a -> b =
  fun atyp btyp -> match coercion atyp btyp with
  | Id -> id
  | Coercion c -> c

let rec coerce_fn : type a b. a fn -> b fn -> a -> b =
  fun afn bfn -> match fn_coercion afn bfn with
  | Id -> id
  | Coercion c -> c
