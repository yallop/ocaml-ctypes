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

type _ typ =
    Void            :                       unit typ
  | Primitive       : 'a Primitives.prim -> 'a typ 
  | Pointer         : 'a typ             -> 'a ptr typ
  | Struct          : 'a structure_type  -> 'a structure typ
  | Union           : 'a union_type      -> 'a union typ
  | Abstract        : abstract_type      -> 'a abstract typ
  | View            : ('a, 'b) view      -> 'a typ
  | Array           : 'a typ * int       -> 'a array typ
and 'a ptr = { reftype      : 'a typ;
               raw_ptr      : Ctypes_raw.raw_pointer;
               pmanaged     : Ctypes_raw.managed_buffer option;
               pbyte_offset : int }
and 'a array = { astart : 'a ptr; alength : int }
and ('a, 'kind) structured = { structured : ('a, 'kind) structured ptr }
and 'a union = ('a, [`Union]) structured
and 'a structure = ('a, [`Struct]) structured
and 'a abstract = ('a, [`Abstract]) structured
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
  (* The flag indicates whether we should check errno *)
  | Returns  : bool * 'a typ   -> 'a fn
  | Function : 'a typ * 'b fn  -> ('a -> 'b) fn

type boxed_typ = BoxedType : 'a typ -> boxed_typ

let rec sizeof : type a. a typ -> int = function
    Void                           -> raise IncompleteType
  | Primitive p                    -> Primitive_details.sizeof p
  | Struct { spec = Incomplete _ } -> raise IncompleteType
  | Struct { spec = Complete
      { size } }                   -> size
  | Union { uspec = None }         -> raise IncompleteType
  | Union { uspec = Some { size } }
                                   -> size
  | Array (t, i)                   -> i * sizeof t
  | Abstract { asize }             -> asize
  | Pointer _                      -> Primitive_details.pointer_size
  | View { ty }                    -> sizeof ty

let rec alignment : type a. a typ -> int = function
    Void                             -> raise IncompleteType
  | Primitive p                      -> Primitive_details.alignment p
  | Struct { spec = Incomplete _ }   -> raise IncompleteType
  | Struct { spec = Complete
      { align } }                    -> align
  | Union { uspec = None }           -> raise IncompleteType
  | Union { uspec = Some { align } } -> align
  | Array (t, _)                     -> alignment t
  | Abstract { aalignment }          -> aalignment
  | Pointer _                        -> Primitive_details.pointer_alignment 
  | View { ty }                      -> alignment ty

let rec passable : type a. a typ -> bool = function
    Void                            -> true
  | Primitive Primitives.Complex32  -> false
  | Primitive Primitives.Complex64  -> false
  | Primitive p                     -> true 
  | Struct { spec = Incomplete _ }  -> raise IncompleteType
  | Struct { fields }               -> List.for_all passable_boxed_field fields
  | Union { uspec = None }          -> raise IncompleteType
  | Union _                         -> false
  | Array _                         -> false
  | Pointer _                       -> true
  | Abstract _                      -> false
  | View { ty }                     -> passable ty
and passable_boxed_field : type s. s boxed_field -> bool =
   fun (BoxedField { ftype } ) -> passable ftype

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
let ptr t = Pointer t
let ( @->) f t =
  if not (passable f) then
    raise (Unsupported "Unsupported argument type")
  else
    Function (f, t)
let abstract ~name ~size ~alignment =
  Abstract { aname = name; asize = size; aalignment = alignment }
let view ?format_typ ~read ~write ty = View { read; write; format_typ; ty }
let returning v =
  if not (passable v) then
    raise (Unsupported "Unsupported return type")
  else
    Returns (false, v)
let returning_checking_errno v = Returns (true, v)

let structure tag =
  Struct { spec = Incomplete { isize = 0 }; tag; fields = [] }

let union utag = Union { utag; uspec = None; ufields = [] }

let offsetof { foffset } = foffset
let field_type { ftype } = ftype
