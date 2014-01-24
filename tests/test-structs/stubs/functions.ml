(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Foreign function bindings for the struct tests. *)

open Ctypes
open Tests_common
open Types

module Stubs (F: FOREIGN) =
struct
  open F

  let accept_struct = foreign "accept_struct"
    (simple @-> returning int)

  let return_struct = foreign "return_struct"
    (void @-> returning simple)
end
