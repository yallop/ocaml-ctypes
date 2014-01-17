(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* C type definitions for the views tests. *)

open Ctypes

let charish = view ~read:Char.chr ~write:Char.code int
let () = Cstubs.register_paths charish
  ~value:"Types.charish" ~typ:"char"

type nullable_intptr = (int -> int -> int) option
let nullable_intptr = Foreign.funptr_opt (int @-> int @-> returning int)

let () = Cstubs.register_paths nullable_intptr
  ~value:"Types.nullable_intptr" ~typ:"Types.nullable_intptr"
