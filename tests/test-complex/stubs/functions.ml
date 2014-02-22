(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Foreign function bindings for the complex number tests. *)

open Ctypes

module Stubs (F: Cstubs.FOREIGN) =
struct
  open F

  let bind typ name =
    foreign name
      (ptr typ @-> ptr typ @-> ptr typ @-> returning void)

  let add_complexd = bind complex64 "add_complexd"
  let mul_complexd = bind complex64 "mul_complexd"
  let add_complexf = bind complex32 "add_complexf"
  let mul_complexf = bind complex32 "mul_complexf"
end
