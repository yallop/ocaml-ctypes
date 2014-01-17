(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* C type definitions for the C standard library tests. *)

open Ctypes
open Foreign

type funptr_comparator = unit ptr -> unit ptr -> int
let funptr_comparator = funptr (ptr void @-> ptr void @-> returning int)
let () = Cstubs.register_paths funptr_comparator
  ~value:"Types.funptr_comparator" ~typ:"Types.funptr_comparator"

let cchar = view ~read:Char.chr ~write:Char.code int
let () = Cstubs.register_paths cchar
  ~value:"Types.cchar" ~typ:"char"

let bool = view ~read:((<>)0) ~write:(fun b -> if b then 1 else 0) int
let () = Cstubs.register_paths bool
  ~value:"Types.bool" ~typ:"bool"
