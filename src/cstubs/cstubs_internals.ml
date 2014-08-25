(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Types and functions used by generated ML code.  This is an internal
   interface and subject to change. *)

type voidp = Ctypes_ptr.voidp
type managed_buffer = Memory_stubs.managed_buffer
type 'a fatptr = 'a Ctypes.typ Ctypes_ptr.Fat.t

let make_structured reftyp buf =
  let open Static in
  let managed = Obj.repr buf in
  let raw_ptr = Memory_stubs.block_address buf in
  { structured = CPointer (Ctypes_ptr.Fat.make ~managed ~reftyp raw_ptr) }

include Static
include Primitives

let make_ptr reftyp raw_ptr = CPointer (Ctypes_ptr.Fat.make ~reftyp raw_ptr)

let cptr (CPointer p) = p
