(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* C type definitions for the callback lifetime tests. *)

open Ctypes
open Foreign

type callback_type_ptr = int -> int
let callback_type_ptr = funptr (int @-> returning int)
let () = Cstubs.register_paths callback_type_ptr
  ~value:"Types.callback_type_ptr" ~typ:"Types.callback_type_ptr"
