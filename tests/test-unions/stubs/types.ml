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
  let u1 : [`u1] union typ = union "u1"
  let x1 = field u1 "x1" char
  let () = seal u1
end
