(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* C type definitions for the pointer tests. *)

open Ctypes
open Foreign

type pintfun1ptr = int ptr -> int ptr -> int
let pintfun1ptr = funptr (ptr int @-> ptr int @-> returning int)
let () = Cstubs.register_paths pintfun1ptr
  ~value:"Types.pintfun1ptr" ~typ:"Types.pintfun1ptr"

type pintfun2ptr = int -> int -> int ptr
let pintfun2ptr = funptr (int @-> int @-> returning (ptr int))
let () = Cstubs.register_paths pintfun2ptr
  ~value:"Types.pintfun2ptr" ~typ:"Types.pintfun2ptr"

type arg_type = int -> int -> int
let arg_type = funptr (int @-> int @-> returning int)
let () = Cstubs.register_paths arg_type
  ~value:"Types.arg_type" ~typ:"Types.arg_type"

type iiifunptr = int -> int -> int
let iiifunptr = funptr (int @-> int @-> returning int)
let () = Cstubs.register_paths iiifunptr
  ~value:"Types.iiifunptr" ~typ:"Types.iiifunptr"
