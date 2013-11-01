(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Coercions *)

open Static

exception Uncoerceable

let rec coerce : type a b. a typ -> b typ -> a -> b =
  fun atyp btyp -> match atyp, btyp with
  | _, Void -> fun _ -> ()
  | View av, b ->
    let coerce = coerce av.ty b in
    fun v -> coerce (av.write v)
  | a, View bv ->
    let coerce = coerce a bv.ty in
    fun v -> bv.read (coerce v)
  | Pointer _, Pointer b ->
    fun v -> Memory.(from_voidp b (to_voidp v))
  | _ -> raise Uncoerceable 
