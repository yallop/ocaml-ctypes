(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* C type definitions for the arrays tests. *)

open Ctypes

(* union u {
      int i;
      double d;
   }
*)
type number
let u : number union typ = union "number"
let (-:) ty label = field u label ty
let i = int    -: "i"
let d = double -: "d"
let () = seal u

(* struct s {
      char tag;
      union u data;
   }
*)
type tagged
let s : tagged structure typ = structure "tagged"
let (-:) ty label = field s label ty
let tag  = char -: "tag"
let data = u    -: "num"
let () = seal s
let () = Cstubs.register_paths s
  ~value:"Types.s" ~typ:"Types.tagged"
