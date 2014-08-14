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

let make_structured reftype buf =
  let open Static in
  let pmanaged = Some (Obj.repr buf) in
  let raw_ptr = Memory_stubs.block_address buf in
  { structured = CPointer { Ctypes_ptr.reftype; pmanaged; raw_ptr; } }

include Static
include Primitives

let make_ptr reftype raw_ptr =
  CPointer { Ctypes_ptr.reftype; raw_ptr; pmanaged = None; }

let raw_ptr (CPointer { Ctypes_ptr.raw_ptr }) = raw_ptr
