(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Static
open Memory

type 'a t = 'a carray

let check_bound { alength } i =
  if i >= alength then
    invalid_arg "index out of bounds"

let unsafe_get { astart } n = !@(astart +@ n)
let unsafe_set { astart } n v = (astart +@ n) <-@ v

let get arr n =
  check_bound arr n;
  unsafe_get arr n

let set arr n v =
  check_bound arr n;
  unsafe_set arr n v

let start { astart } = astart
let length { alength } = alength
let from_ptr astart alength = { astart; alength }

let fill ({ alength } as arr) v =
  for i = 0 to alength - 1 do unsafe_set arr i v done

let make : type a. ?finalise:(a t -> unit) -> a typ -> ?initial:a -> int -> a t
  = fun ?finalise reftype ?initial count ->
    let finalise = match finalise with
      | Some f -> Some (fun astart -> f { astart; alength = count } )
      | None -> None
    in
    let arr = { astart = allocate_n ?finalise ~count reftype;
                alength = count } in
    match initial with
      | None -> arr
      | Some v -> fill arr v; arr

let element_type { astart } = reference_type astart 

let of_list typ list =
  let arr = make typ (List.length list) in
  List.iteri (set arr) list;
  arr

let to_list a =
  let l = ref [] in
  for i = length a - 1 downto 0 do
    l := get a i :: !l
  done;
  !l

let iter (type a) (f : a -> unit) a =
  for i = 0 to length a - 1 do
    f (unsafe_get a i)
  done

let iteri (type a) (f : int -> a -> unit) a =
  for i = 0 to length a - 1 do
    f i (unsafe_get a i)
  done

let map (type a) (type b) (ty : b typ) (f : a -> b) ({ alength } as a : a t) =
  let result = make ty alength in
  for i = 0 to alength - 1 do
    unsafe_set result i (f (unsafe_get a i))
  done;
  result

let mapi (type a) (type b) (ty : b typ) (f : int -> a -> b)
    ({ alength } as a : a t) =
  let result = make ty alength in
  for i = 0 to alength - 1 do
    unsafe_set result i (f i (unsafe_get a i))
  done;
  result

let fold_left (type a) (type b) (f : a -> b -> a) (u : a) a =
  let result = ref u in
  for i = 0 to length a - 1 do
    result := f !result (unsafe_get a i)
  done;
  !result

let fold_right (type a) (type b) (f : b -> a -> a) a (u : a) =
  let result = ref u in
  for i = length a - 1 downto 0 do
    result := f (unsafe_get a i) !result
  done;
  !result

open Bigarray

let to_bigarray spec kind a =
  let dims = array_dims spec a in
  !@ (castp (bigarray spec dims kind) (start a))

let of_bigarray : type a b c d e.
  < element: a;
    ba_repr: e;
    bigarray: b;
    carray: c;
    dims: d > bigarray_class -> b -> c
  = fun spec ba -> 
    let CPointer { reftype } as element_ptr = bigarray_start spec ba in
    match spec with
  | Genarray ->
    let ds = Genarray.dims ba in
    from_ptr element_ptr (Array.fold_left ( * ) 1 ds)
  | Array1 ->
    let d = Array1.dim ba in
    from_ptr element_ptr d
  | Array2 ->
    let d1 = Array2.dim1 ba and d2 = Array2.dim2 ba in
    from_ptr (castp (array d2 reftype) element_ptr) d1
  | Array3 ->
    let d1 = Array3.dim1 ba and d2 = Array3.dim2 ba and d3 = Array3.dim3 ba in
    from_ptr (castp (array d2 (array d3 reftype)) element_ptr) d1
