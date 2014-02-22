(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Foreign function bindings for the struct tests. *)

open Ctypes

module Stubs (F: Cstubs.FOREIGN) =
struct
  open F

  type simple
  let simple : simple structure typ = structure "simple"
  let i = field simple "i" int
  let f = field simple "f" double
  let self = field simple "self" (ptr simple)
  let () = seal simple

  let accept_struct = foreign "accept_struct"
    (simple @-> returning int)

  let return_struct = foreign "return_struct"
    (void @-> returning simple)
end
