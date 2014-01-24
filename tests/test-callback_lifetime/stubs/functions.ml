(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Foreign function bindings for the callback lifetime tests. *)

open Ctypes
open Tests_common
open Types

module Stubs (F: FOREIGN) =
struct
  open F

  let store_callback = foreign "store_callback"
    (callback_type_ptr @-> returning void)

  let invoke_stored_callback = foreign "invoke_stored_callback"
    (int @-> returning int)

  let return_callback = foreign "return_callback"
    (callback_type_ptr @-> returning callback_type_ptr)
end
