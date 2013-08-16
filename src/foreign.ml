(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Dl
open Ctypes

let ptr_of_raw_ptr p = 
  Ctypes.ptr_of_raw_address (Ctypes_raw.PtrType.to_int64 p)

let foreign_value ?from symbol t =
  from_voidp t (ptr_of_raw_ptr (dlsym ?handle:from ~symbol))

let foreign ?from symbol typ = let open Ctypes in
  let p = ptr_of_raw_ptr (dlsym ?handle:from ~symbol) in
  let pp = to_voidp (allocate (ptr void) p) in
  !@ (from_voidp (funptr ~name:symbol typ) pp)
