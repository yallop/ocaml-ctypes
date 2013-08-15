(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Stubs for the dynamic semantics. *)

open Ctypes_raw

(* Allocate a region of stable memory managed by a custom block. *)
external allocate : int -> managed_buffer
  = "ctypes_allocate"

(* Obtain the address of the managed block. *)
external block_address : managed_buffer -> raw_pointer
  = "ctypes_block_address"

(* Read a C value from a block of memory *)
external read : 'a Types.ctype_io -> offset:int -> raw_pointer -> 'a
  = "ctypes_read"

(* Write a C value to a block of memory *)
external write :  'a Types.ctype_io -> offset:int -> 'a -> raw_pointer -> unit
  = "ctypes_write"

(* Copy [size] bytes from [src + src_offset] to [dst + dst_offset]. *)
external memcpy :
  dst:raw_pointer -> dst_offset:int ->
  src:raw_pointer -> src_offset:int ->
    size:int -> unit
  = "ctypes_memcpy"


(* Return a string representation of a C value *)
external string_of : 'a Types.ctype_io -> 'a -> string =
    "ctypes_string_of"
