(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Ctypes

module Struct_stubs(S : Ctypes_types.TYPE) =
struct
  open S

  (* missing fields *)
  let s1 : [`s1] structure typ = structure "s1"
  let x1 = field s1 "x1" int
  let x4 = field s1 "x4" int
  let () = seal s1

  (* fields reordered *)
  let s2 : [`s2] structure typ = structure "s2"
  let y2 = field s2 "y2" int
  let y1 = field s2 "y1" int
  let () = seal s2

  (* one struct depending on another *)
  let s3 : [`s3] structure typ = structure "s3"
  let z1 = field s3 "z1" int
  let z2 = field s3 "z2" (ptr s3)
  let () = seal s3

  let s4 : [`s4] structure typ = structure "s4"
  let z3 = field s4 "z3" s3
  let z4 = field s4 "z4" (ptr s3)
  let () = seal s4
end
