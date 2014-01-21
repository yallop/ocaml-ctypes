(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Types and functions used by generated ML code.  This is an internal
   interface and subject to change. *)

open Ctypes

type voidp = Ctypes_raw.voidp
type managed_buffer = Memory_stubs.managed_buffer

val make_structured :
  ('a, 's) structured typ -> managed_buffer -> ('a, 's) structured

type 'a ptr = 'a Static.ptr
  = { reftype      : 'a typ;
      raw_ptr      : voidp;
      pmanaged     : Obj.t option;
      pbyte_offset : int }

val make_ptr : 'a typ -> voidp -> 'a ptr
