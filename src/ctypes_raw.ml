(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Boxed pointers to C memory locations . *)

module PtrType = (val match Ctypes_primitives.pointer_size with
  4 -> (module Signed.Int32 : Signed.S)
| 8 -> (module Signed.Int64 : Signed.S)
| _ -> failwith "No suitable type available to represent pointers.")

type voidp = PtrType.t

let null = PtrType.zero



