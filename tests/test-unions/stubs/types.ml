(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* C type definitions for the union tests. *)

open Ctypes

type padded
let padded : padded union typ = union "padded"
let (-:) ty label = field padded label ty
let i = int64_t                         -: "i"
let a = array (sizeof int64_t + 1) char -: "a"
let () = seal padded
let () = Cstubs.register_paths padded
  ~value:"Types.padded" ~typ:"Types.padded"
