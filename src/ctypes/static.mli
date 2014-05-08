(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* C type construction.  Internal representation, not for public use. *)

type abstract_type = {
  aname : string;
  asize : int;
  aalignment : int;
}

type _ ocaml_type =
  String     : string ocaml_type
| Bytes      : Bytes.t ocaml_type
| FloatArray : float array ocaml_type

type incomplete_size = { mutable isize: int }

type structured_spec = { size: int; align: int; }

type 'a structspec =
    Incomplete of incomplete_size
  | Complete of structured_spec

type _ typ =
    Void            :                       unit typ
  | Primitive       : 'a Primitives.prim -> 'a typ
  | Pointer         : 'a typ             -> 'a ptr typ
  | Struct          : 'a structure_type  -> 'a structure typ
  | Union           : 'a union_type      -> 'a union typ
  | Abstract        : abstract_type      -> 'a abstract typ
  | View            : ('a, 'b) view      -> 'a typ
  | Array           : 'a typ * int       -> 'a carray typ
  | Bigarray        : (_, 'a) Ctypes_bigarray.t
                                         -> 'a typ
  | OCaml           : 'a ocaml_type      -> 'a ocaml typ
and 'a cptr = { reftype      : 'a typ;
                raw_ptr      : Ctypes_raw.voidp;
                pmanaged     : Obj.t option;
                pbyte_offset : int; }
and 'a carray = { astart : 'a ptr; alength : int }
and ('a, 'kind) structured = { structured : ('a, 'kind) structured ptr }
and 'a union = ('a, [`Union]) structured
and 'a structure = ('a, [`Struct]) structured
and 'a abstract = ('a, [`Abstract]) structured
and (_, _) pointer =
  CPointer : 'a cptr -> ('a, [`C]) pointer
| OCamlRef : int * 'a * 'a ocaml_type -> ('a, [`OCaml]) pointer
and 'a ptr = ('a, [`C]) pointer
and 'a ocaml = ('a, [`OCaml]) pointer
and ('a, 'b) view = {
  read : 'b -> 'a;
  write : 'a -> 'b;
  format_typ: ((Format.formatter -> unit) -> Format.formatter -> unit) option;
  ty: 'b typ;
}
and ('a, 's) field = {
  ftype: 'a typ;
  foffset: int;
  fname: string;
}
and 'a structure_type = {
  tag: string;
  mutable spec: 'a structspec;
  mutable fields : 'a structure boxed_field list;
}
and 'a union_type = {
  utag: string;
  mutable uspec: structured_spec option;
  mutable ufields : 'a union boxed_field list;
}
and 's boxed_field = BoxedField : ('a, 's) field -> 's boxed_field

type _ bigarray_class =
  Genarray :
  < element: 'a;
    dims: int array;
    ba_repr: 'b;
    bigarray: ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t;
    carray: 'a carray > bigarray_class
| Array1 :
  < element: 'a;
    dims: int;
    ba_repr: 'b;
    bigarray: ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t;
    carray: 'a carray > bigarray_class
| Array2 :
  < element: 'a;
    dims: int * int;
    ba_repr: 'b;
    bigarray: ('a, 'b, Bigarray.c_layout) Bigarray.Array2.t;
    carray: 'a carray carray > bigarray_class
| Array3 :
  < element: 'a;
    dims: int * int * int;
    ba_repr: 'b;
    bigarray: ('a, 'b, Bigarray.c_layout) Bigarray.Array3.t;
    carray: 'a carray carray carray > bigarray_class

type _ fn =
  | Returns  : 'a typ   -> 'a fn
  | Function : 'a typ * 'b fn  -> ('a -> 'b) fn

type boxed_typ = BoxedType : 'a typ -> boxed_typ

val sizeof : 'a typ -> int
val alignment : 'a typ -> int
val passable : 'a typ -> bool

val void : unit typ
val char : char typ
val schar : int typ
val float : float typ
val double : float typ
val complex32 : Complex.t typ
val complex64 : Complex.t typ
val short : int typ
val int : int typ
val long : Signed.long typ
val llong : Signed.llong typ
val nativeint : nativeint typ
val int8_t : int typ
val int16_t : int typ
val int32_t : Signed.Int32.t typ
val int64_t : Signed.Int64.t typ
val camlint : int typ
val uchar : Unsigned.uchar typ
val uint8_t : Unsigned.UInt8.t typ
val uint16_t : Unsigned.UInt16.t typ
val uint32_t : Unsigned.UInt32.t typ
val uint64_t : Unsigned.UInt64.t typ
val size_t : Unsigned.size_t typ
val ushort : Unsigned.ushort typ
val uint : Unsigned.uint typ
val ulong : Unsigned.ulong typ
val ullong : Unsigned.ullong typ
val array : int -> 'a typ -> 'a carray typ
val ocaml_string : string ocaml typ
val ocaml_bytes : Bytes.t ocaml typ
val ocaml_float_array : float array ocaml typ
val ptr : 'a typ -> 'a ptr typ
val ( @-> ) : 'a typ -> 'b fn -> ('a -> 'b) fn
val abstract : name:string -> size:int -> alignment:int -> 'a abstract typ
val view : ?format_typ:((Format.formatter -> unit) ->
                        Format.formatter -> unit) ->
           read:('a -> 'b) -> write:('b -> 'a) -> 'a typ -> 'b typ
val bigarray : < ba_repr : 'c;
                 bigarray : 'd;
                 carray : 'e;
                 dims : 'b;
                 element : 'a > bigarray_class ->
               'b -> ('a, 'c) Bigarray.kind -> 'd typ
val returning : 'a typ -> 'a fn
val structure : string -> 'a structure typ
val union : string -> 'a union typ
val offsetof : ('a, 'b) field -> int
val field_type : ('a, 'b) field -> 'a typ

exception IncompleteType
exception ModifyingSealedType of string
exception Unsupported of string

val unsupported : ('a, unit, string, _) format4 -> 'a
