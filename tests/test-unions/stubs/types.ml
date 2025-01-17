(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Ctypes

module Struct_stubs(S : Ctypes.TYPE) =
struct
  open S

  (* missing fields *)
  let u1 : [`u1] union typ = union "u1"
  let x1 = field u1 "x1" char
  let () = seal u1

  (* adding fields through views (typedefs) *)
  let union_u2 : [`u2] union typ = union ""
  let u2 = typedef union_u2 "u2"
  let t1 = field u2 "t1" int
  let t2 = field u2 "t2" float
  let () = seal u2

  (* adding fields through views (typedefs) *)
  let union_u3 : [`u3] union typ = union ""
  let u3 = typedef union_u3 "u3"
  let u3_t1 = field u3 "t1" int
  let u3_t2 = field u3 "t2" float
  let u3_t3 = field u3 "t3" double
  let () = seal u3
end
