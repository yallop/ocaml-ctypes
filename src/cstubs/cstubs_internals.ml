(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Types and functions used by generated ML code.  This is an internal
   interface and subject to change. *)

type voidp = Ctypes_raw.voidp
type managed_buffer = Memory_stubs.managed_buffer

let make_structured reftype buf =
  let open Static in
  let pmanaged = Some (Obj.repr buf) in
  let raw_ptr = Memory_stubs.block_address buf in
  let pbyte_offset = 0 in
  { structured = { reftype; pmanaged; pbyte_offset; raw_ptr } }

include Static
include Primitives

let make_ptr reftype raw_ptr =
  { reftype; raw_ptr; pmanaged = None; pbyte_offset = 0 }
