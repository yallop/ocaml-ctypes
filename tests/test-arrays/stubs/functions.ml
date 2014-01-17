(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Foreign function bindings for the arrays tests. *)

open Ctypes
open Tests_common
open Types

module Stubs (F: FOREIGN) =
struct
  open F

  let accepts_pointer_to_array_of_structs =
    foreign "accepts_pointer_to_array_of_structs"
      (ptr (array 5 s) @-> returning double)
end
