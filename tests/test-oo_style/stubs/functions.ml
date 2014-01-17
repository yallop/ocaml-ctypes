(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Foreign function bindings for the OO-style tests. *)

open Ctypes
open Tests_common
open Types

module Stubs (F: FOREIGN) =
struct
  open F

  let check_name = foreign "check_name"
    (ptr animal @-> string @-> returning int)

    let new_chorse = foreign "new_chorse"
      (int @-> returning (ptr animal))
end
