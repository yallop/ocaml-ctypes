(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Format

val format_typ : formatter -> 'a Ctypes.typ -> unit
val format_fn : formatter -> 'a Ctypes.fn -> unit
val format_sint : formatter -> Signed.SInt.t -> unit
val format_long : formatter -> Signed.Long.t -> unit
val format_llong : formatter -> Signed.LLong.t -> unit
val format_uchar : formatter -> Unsigned.UChar.t -> unit
val format_uint8 : formatter -> Unsigned.UInt8.t -> unit
val format_uint16 : formatter -> Unsigned.UInt16.t -> unit
val format_uint32 : formatter -> Unsigned.UInt32.t -> unit
val format_uint64 : formatter -> Unsigned.UInt64.t -> unit
val format_ushort : formatter -> Unsigned.UShort.t -> unit
val format_uint : formatter -> Unsigned.UInt.t -> unit
val format_ulong : formatter -> Unsigned.ULong.t -> unit
val format_ullong : formatter -> Unsigned.ULLong.t -> unit
val format_pointer : formatter -> 'a Ctypes.ptr -> unit
val format_struct : formatter -> ('a, 'b) Ctypes.structured -> unit
val format_union : formatter -> ('a, 'b) Ctypes.structured -> unit
val format_array : formatter -> 'a Ctypes.CArray.t -> unit
val format_ocaml : formatter -> 'a Ctypes.ocaml -> unit
val format_ldouble : formatter -> LDouble.t -> unit
val format_complexld : formatter -> ComplexL.t -> unit
