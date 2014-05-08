(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Types and functions used by generated ML code.  This is an internal
   interface and subject to change. *)

open Ctypes
open Signed
open Unsigned

type voidp = Ctypes_raw.voidp
type managed_buffer = Memory_stubs.managed_buffer

val make_structured :
  ('a, 's) structured typ -> managed_buffer -> ('a, 's) structured

val make_ptr : 'a typ -> voidp -> 'a ptr

val raw_ptr : 'a ptr -> voidp

type 'a ocaml_type = 'a Static.ocaml_type =
  String     : string ocaml_type
| Bytes      : Bytes.t ocaml_type
| FloatArray : float array ocaml_type

type 'a typ = 'a Static.typ =
    Void            :                              unit typ
  | Primitive       : 'a Primitives.prim        -> 'a typ
  | Pointer         : 'a typ                    -> 'a ptr typ
  | Struct          : 'a Static.structure_type  -> 'a Static.structure typ
  | Union           : 'a Static.union_type      -> 'a Static.union typ
  | Abstract        : Static.abstract_type      -> 'a Static.abstract typ
  | View            : ('a, 'b) view             -> 'a typ
  | Array           : 'a typ * int              -> 'a Static.carray typ
  | Bigarray        : (_, 'a) Ctypes_bigarray.t -> 'a typ
  | OCaml           : 'a ocaml_type             -> 'a ocaml typ
and 'a cptr = 'a Static.cptr
  = { reftype      : 'a typ;
      raw_ptr      : voidp;
      pmanaged     : Obj.t option;
      pbyte_offset : int; }
and ('a, 'b) pointer = ('a, 'b) Static.pointer =
  CPointer : 'a cptr -> ('a, [`C]) pointer
| OCamlRef : int * 'a * 'a ocaml_type -> ('a, [`OCaml]) pointer
and 'a ptr = ('a, [`C]) pointer
and 'a ocaml = ('a, [`OCaml]) pointer
and ('a, 'b) view = ('a, 'b) Static.view = {
  read : 'b -> 'a;
  write : 'a -> 'b;
  format_typ: ((Format.formatter -> unit) -> Format.formatter -> unit) option;
  ty: 'b typ;
}

type 'a fn = 'a Static.fn =
  | Returns  : 'a typ   -> 'a fn
  | Function : 'a typ * 'b fn  -> ('a -> 'b) fn

type 'a prim = 'a Primitives.prim =
  Char : char prim
| Schar : int prim
| Uchar : uchar prim
| Short : int prim
| Int : int prim
| Long : long prim
| Llong : llong prim
| Ushort : ushort prim
| Uint : uint prim
| Ulong : ulong prim
| Ullong : ullong prim
| Size_t : size_t prim
| Int8_t : int prim
| Int16_t : int prim
| Int32_t : int32 prim
| Int64_t : int64 prim
| Uint8_t : uint8 prim
| Uint16_t : uint16 prim
| Uint32_t : uint32 prim
| Uint64_t : uint64 prim
| Camlint : int prim
| Nativeint : nativeint prim
| Float : float prim
| Double : float prim
| Complex32 : Complex.t prim
| Complex64 : Complex.t prim
