(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* C type construction *)

exception IncompleteType
exception ModifyingSealedType of string
exception Unsupported of string

let unsupported fmt = Printf.ksprintf (fun s -> raise (Unsupported s)) fmt

type incomplete_size = { mutable isize: int }

type structured_spec = { size: int; align: int; }

type 'a structspec =
    Incomplete of incomplete_size
  | Complete of structured_spec

type abstract_type = {
  aname : string;
  asize : int;
  aalignment : int;
}

type _ ocaml_type =
  String     : string ocaml_type
| Bytes      : Bytes.t ocaml_type
| FloatArray : float array ocaml_type

type _ typ =
    Void            :                       unit typ
  | Primitive       : 'a Primitives.prim -> 'a typ
  | Pointer         : 'a typ             -> 'a ptr typ
  | Struct          : 'a structure_type  -> 'a structure typ
  | Union           : 'a union_type      -> 'a union typ
  | Abstract        : abstract_type      -> 'a abstract typ
  | View            : ('a, 'b) view      -> 'a typ
  | Array           : 'a typ * int       -> 'a carray typ
  | OCaml           : 'a ocaml_type      -> 'a ocaml typ
and 'a carray = { astart : 'a ptr; alength : int }
and ('a, 'kind) structured = { structured : ('a, 'kind) structured ptr }
and 'a union = ('a, [`Union]) structured
and 'a structure = ('a, [`Struct]) structured
and 'a abstract = ('a, [`Abstract]) structured
and (_, _) pointer =
  CPointer : 'a typ Ctypes_ptr.Fat.t -> ('a, [`C]) pointer
| OCamlRef : int * 'a * 'a ocaml_type -> ('a, [`OCaml]) pointer
and 'a ptr = ('a, [`C]) pointer
and 'a ocaml = ('a, [`OCaml]) pointer
and ('a, 'b) view = {
  read : 'b -> 'a;
  write : 'a -> 'b;
  format_typ: ((Format.formatter -> unit) -> Format.formatter -> unit) option;
  format: (Format.formatter -> 'a -> unit) option;
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
  (* fields are in reverse order iff the struct type is incomplete *)
  mutable fields : 'a structure boxed_field list;
}
and 'a union_type = {
  utag: string;
  mutable uspec: structured_spec option;
  (* fields are in reverse order iff the union type is incomplete *)
  mutable ufields : 'a union boxed_field list;
}
and 's boxed_field = BoxedField : ('a, 's) field -> 's boxed_field

type _ fn =
  | Returns  : 'a typ   -> 'a fn
  | Function : 'a typ * 'b fn  -> ('a -> 'b) fn

type boxed_typ = BoxedType : 'a typ -> boxed_typ

let rec sizeof : type a. a typ -> int = function
    Void                           -> raise IncompleteType
  | Primitive p                    -> Ctypes_primitives.sizeof p
  | Struct { spec = Incomplete _ } -> raise IncompleteType
  | Struct { spec = Complete
      { size } }                   -> size
  | Union { uspec = None }         -> raise IncompleteType
  | Union { uspec = Some { size } }
                                   -> size
  | Array (t, i)                   -> i * sizeof t
  | Abstract { asize }             -> asize
  | Pointer _                      -> Ctypes_primitives.pointer_size
  | OCaml _                        -> raise IncompleteType
  | View { ty }                    -> sizeof ty

let rec alignment : type a. a typ -> int = function
    Void                             -> raise IncompleteType
  | Primitive p                      -> Ctypes_primitives.alignment p
  | Struct { spec = Incomplete _ }   -> raise IncompleteType
  | Struct { spec = Complete
      { align } }                    -> align
  | Union { uspec = None }           -> raise IncompleteType
  | Union { uspec = Some { align } } -> align
  | Array (t, _)                     -> alignment t
  | Abstract { aalignment }          -> aalignment
  | Pointer _                        -> Ctypes_primitives.pointer_alignment
  | OCaml _                          -> raise IncompleteType
  | View { ty }                      -> alignment ty

let rec passable : type a. a typ -> bool = function
    Void                           -> true
  | Primitive _                    -> true
  | Struct { spec = Incomplete _ } -> raise IncompleteType
  | Struct { spec = Complete _ }   -> true
  | Union  { uspec = None }        -> raise IncompleteType
  | Union  { uspec = Some _ }      -> true
  | Array _                        -> false
  | Pointer _                      -> true
  | Abstract _                     -> false
  | OCaml _                        -> true
  | View { ty }                    -> passable ty

let void = Void
let char = Primitive Primitives.Char
let schar = Primitive Primitives.Schar
let float = Primitive Primitives.Float
let double = Primitive Primitives.Double
let complex32 = Primitive Primitives.Complex32
let complex64 = Primitive Primitives.Complex64
let short = Primitive Primitives.Short
let int = Primitive Primitives.Int
let long = Primitive Primitives.Long
let llong = Primitive Primitives.Llong
let nativeint = Primitive Primitives.Nativeint
let int8_t = Primitive Primitives.Int8_t
let int16_t = Primitive Primitives.Int16_t
let int32_t = Primitive Primitives.Int32_t
let int64_t = Primitive Primitives.Int64_t
let camlint = Primitive Primitives.Camlint
let uchar = Primitive Primitives.Uchar
let uint8_t = Primitive Primitives.Uint8_t
let uint16_t = Primitive Primitives.Uint16_t
let uint32_t = Primitive Primitives.Uint32_t
let uint64_t = Primitive Primitives.Uint64_t
let size_t = Primitive Primitives.Size_t
let ushort = Primitive Primitives.Ushort
let uint = Primitive Primitives.Uint
let ulong = Primitive Primitives.Ulong
let ullong = Primitive Primitives.Ullong
let array i t = Array (t, i)
let ocaml_string = OCaml String
let ocaml_bytes = OCaml Bytes
let ocaml_float_array = OCaml FloatArray
let ptr t = Pointer t
let ( @->) f t =
  if not (passable f) then
    raise (Unsupported "Unsupported argument type")
  else
    Function (f, t)
let abstract ~name ~size ~alignment =
  Abstract { aname = name; asize = size; aalignment = alignment }
let view ?format_typ ?format ~read ~write ty =
  View { read; write; format_typ; format; ty }
let returning v =
  if not (passable v) then
    raise (Unsupported "Unsupported return type")
  else
    Returns v

let structure tag =
  Struct { spec = Incomplete { isize = 0 }; tag; fields = [] }

let union utag = Union { utag; uspec = None; ufields = [] }

let offsetof { foffset } = foffset
let field_type { ftype } = ftype
let field_name { fname } = fname
