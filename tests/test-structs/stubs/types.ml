(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* C type definitions for the struct tests. *)

open Ctypes
open Foreign

type simple
let simple : simple structure typ = structure "simple"
let i = field simple "i" int
let f = field simple "f" double
let self = field simple "self" (ptr simple)
let () = seal simple
let () = Cstubs.register_paths simple
  ~value:"Types.simple" ~typ:"Types.simple"
