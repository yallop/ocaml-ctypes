(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Format

val format_typ : formatter -> 'a Ctypes.typ -> unit
val format_fn : formatter -> 'a Ctypes.fn -> unit
val format_pointer : formatter -> 'a Ctypes.ptr -> unit
val format_struct : formatter -> ('a, 'b) Ctypes.structured -> unit
val format_union : formatter -> ('a, 'b) Ctypes.structured -> unit
val format_array : formatter -> 'a Ctypes.CArray.t -> unit
