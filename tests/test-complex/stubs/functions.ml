(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Foreign function bindings for the complex number tests. *)

open Ctypes

(* These functions can be bound either dynamically using Foreign or statically
   using stub generation. *)
module Common(F : Cstubs.FOREIGN) =
struct
  let bind typ name =
    F.(foreign name
         (ptr typ @-> ptr typ @-> ptr typ @-> returning void))

  let add_complexd = bind complex64 "add_complexd"
  let mul_complexd = bind complex64 "mul_complexd"
  let add_complexf = bind complex32 "add_complexf"
  let mul_complexf = bind complex32 "mul_complexf"
end

(* These functions can only be bound using stub generation, since Foreign
   doesn't support passing complex numbers by value. *)
module Stubs_only(F : Cstubs.FOREIGN) =
struct
  let bind typ name =
    F.(foreign name (typ @-> typ @-> returning typ))

  let add_complexd_val = bind complex64 "add_complexd_val"
  let mul_complexd_val = bind complex64 "mul_complexd_val"
  let add_complexf_val = bind complex32 "add_complexf_val"
  let mul_complexf_val = bind complex32 "mul_complexf_val"
end

module Stubs (F: Cstubs.FOREIGN) =
struct
  include Common(F)
  include Stubs_only(F)
end
