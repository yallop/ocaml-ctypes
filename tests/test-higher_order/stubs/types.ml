(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* C type definitions for the higher order tests. *)

open Ctypes
open Foreign

type intfunptr = int -> int -> int
let intfun = (int @-> int @-> returning int)
let intfunptr = funptr (int @-> int @-> returning int)

let () = Cstubs.register_paths intfunptr
  ~value:"Types.intfunptr" ~typ:"Types.intfunptr"

let acceptor = (funptr intfun @-> int @-> int @-> returning int)
type funptr_acceptor = (int -> int -> int) -> int -> int -> int
let funptr_acceptor = funptr acceptor
let () = Cstubs.register_paths funptr_acceptor
  ~value:"Types.funptr_acceptor" ~typ:"Types.funptr_acceptor"

let intfun1 = (int @-> returning int)
let intfun1ptr = funptr intfun1
type intfun1ptr = int -> int
let () = Cstubs.register_paths intfun1ptr
  ~value:"Types.intfun1ptr" ~typ:"Types.intfun1ptr"

let intfunhoptr = funptr (int @-> returning (intfun1ptr))
type intfunhoptr = int -> int -> int
let () = Cstubs.register_paths intfunhoptr
  ~value:"Types.intfunhoptr" ~typ:"Types.intfunhoptr"


