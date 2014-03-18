(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Foreign function bindings for the union tests. *)

open Ctypes

type padded
let padded : padded union typ = union "padded"
let (-:) ty label = field padded label ty
let i = int64_t                         -: "i"
let a = array (sizeof int64_t + 1) char -: "a"
let () = seal padded

module Stubs (F: Cstubs.FOREIGN) =
struct
  open F

  let sum_union_components = foreign "sum_union_components"
    (ptr padded @-> size_t @-> returning int64_t)
end
