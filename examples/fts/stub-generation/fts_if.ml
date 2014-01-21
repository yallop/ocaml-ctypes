(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Ctypes
open Fts_types
open Fts_generated
open FTS
open FTSENT

let crush_options f : 'a list -> int = List.fold_left (fun i o -> i lor (f o)) 0

let fts_read fts =
  let p = fts_read fts.ptr in
  if to_voidp p = null then None
  else Some p

let fts_close ftsp =
  ignore (fts_close ftsp.ptr)
      
let fts_set ~ftsp ~f ~options =
  ignore (fts_set ftsp.ptr f (crush_options fts_set_option_value options))

let fts_children ~ftsp ~name_only =
  fts_children ftsp.ptr (fts_children_option_of_bool name_only)

let null_terminated_array_of_ptr_list typ list =
  let nitems = List.length list in
  let arr = CArray.make typ (1 + nitems) in
  List.iteri (CArray.set arr) list;
  (coerce (ptr string) (ptr (ptr void)) (CArray.start arr +@ nitems)) <-@ null;
  arr

let fts_open ~path_argv ?compar ~options = 
  let paths = null_terminated_array_of_ptr_list string path_argv in
  let options = crush_options fts_open_option_value options in
  { ptr = fts_open (CArray.start paths) options compar; compar }
