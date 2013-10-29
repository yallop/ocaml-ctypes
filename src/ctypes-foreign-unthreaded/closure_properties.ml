(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

module HashPhysical = Hashtbl.Make
  (struct
    type t = Obj.t
    let hash = Hashtbl.hash
    let equal = (==)
   end)

(* Map integer identifiers to functions. *)
let function_by_id = Hashtbl.create 10

(* Map functions (not closures) to identifiers. *)
let id_by_function = HashPhysical.create 10

let store_non_closure_function fn boxed_fn id =
  try
    (* Return the existing identifier, if any. *)
    HashPhysical.find id_by_function fn
  with Not_found ->
    (* Add entries to both tables *)
    HashPhysical.add id_by_function fn id;
    Hashtbl.add function_by_id id boxed_fn;
    id

let fresh () = Oo.id (object end)

let finalise key fn =
  Hashtbl.remove function_by_id key

let record closure boxed_closure : int =
  let key = fresh () in
  try
    (* For closures we add an entry to function_by_id and a finaliser that
       removes the entry. *)
    Gc.finalise (finalise key) closure;
    Hashtbl.add function_by_id key boxed_closure;
    key
  with Invalid_argument "Gc.finalise" ->
    (* For non-closures we add entries to function_by_id and
       id_by_function. *)
      store_non_closure_function closure boxed_closure key

let retrieve id = Hashtbl.find function_by_id id
