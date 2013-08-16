(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Interface to primitive types defined in C, and some exceptions. *)

module PtrType = (val match Primitive_details.pointer_size with
  4 -> (module Signed.Int32 : Signed.S)
| 8 -> (module Signed.Int64 : Signed.S)
| _ -> failwith "No suitable type available to represent pointers.")
type voidp = PtrType.t
let null = PtrType.zero

(* A raw pointer to a block of memory, not managed by the garbage collector,
   into which we can read/write C values. *)
type raw_pointer = voidp

type managed_buffer
