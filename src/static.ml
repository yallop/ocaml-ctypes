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

module RawTypes = Ctypes_raw.Types

type incomplete_size = { mutable isize: int }

type 'a structspec =
    Incomplete of incomplete_size
  | Complete of 'a Static_stubs.structure RawTypes.ctype

type unionspec = { usize: int; ualignment: int }

type arg_type = ArgType : 'b RawTypes.ctype_io -> arg_type

type abstract_type = {
  aname : string;
  asize : int;
  aalignment : int;
}

type _ typ =
    Void            :                      unit typ
  | Primitive       : 'a RawTypes.ctype -> 'a typ
  | Pointer         : 'a typ            -> 'a ptr typ
  | Struct          : 'a structure_type -> 'a structure typ
  | Union           : 'a union_type     -> 'a union typ
  | Abstract        : abstract_type     -> 'a abstract typ
  | View            : ('a, 'b) view     -> 'a typ
  | Array           : 'a typ * int      -> 'a array typ
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
  mutable uspec: unionspec option;
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
  | Primitive { RawTypes.size }    -> size
  | Struct { spec = Incomplete _ } -> raise IncompleteType
  | Struct { spec = Complete
               { RawTypes.size } } -> size
  | Union { uspec = None }         -> raise IncompleteType
  | Union { uspec = Some { usize } }
                                   -> usize
  | Array (t, i)                   -> i * sizeof t
  | Abstract { asize }             -> asize
  | Pointer _                      -> RawTypes.(pointer.size)
  | View { ty }                    -> sizeof ty

let rec alignment : type a. a typ -> int = function
    Void                             -> raise IncompleteType
  | Primitive { RawTypes.alignment } -> alignment
  | Struct { spec = Incomplete _ }   -> raise IncompleteType
  | Struct { spec = Complete
             {RawTypes.alignment } } -> alignment
  | Union { uspec = None }      -> raise IncompleteType
  | Union { uspec = Some { ualignment } }
                                     -> ualignment
  | Array (t, _)                     -> alignment t
  | Abstract { aalignment }          -> aalignment
  | Pointer _                        -> RawTypes.(pointer.alignment)
  | View { ty }                      -> alignment ty

let rec passable : type a. a typ -> bool = function
    Void                            -> true
  | Primitive { RawTypes.passable } -> passable
  | Struct { spec = Incomplete _ }  -> raise IncompleteType
  | Struct { spec = Complete
      { RawTypes.passable } }       -> passable
  | Union { uspec = None }          -> raise IncompleteType
  | Union _                         -> false
  | Array _                         -> false
  | Pointer _                       -> true
  | Abstract _                      -> false
  | View { ty }                     -> passable ty

let rec arg_type : type a. a typ -> arg_type = function
    Void                           -> ArgType RawTypes.(void.raw)
  | Primitive p                    -> ArgType p.RawTypes.raw
  | Struct { spec = Complete p }   -> ArgType p.RawTypes.raw
  | Pointer _                      -> ArgType RawTypes.(pointer.raw)
  | View { ty }                    -> arg_type ty
  (* The following cases should never happen; aggregate types other than
     complete struct types are excluded during type construction. *)
  | Union _                        -> assert false
  | Array _                        -> assert false
  | Abstract _                     -> assert false
  | Struct { spec = Incomplete _ } -> assert false

let void = Void
let char = Primitive RawTypes.char
let schar = Primitive RawTypes.schar
let float = Primitive RawTypes.float
let double = Primitive RawTypes.double
let complex32 = Primitive RawTypes.complex32
let complex64 = Primitive RawTypes.complex64
let short = Primitive RawTypes.short
let int = Primitive RawTypes.int
let long = Primitive RawTypes.long
let llong = Primitive RawTypes.llong
let nativeint = Primitive RawTypes.nativeint
let int8_t = Primitive RawTypes.int8_t
let int16_t = Primitive RawTypes.int16_t
let int32_t = Primitive RawTypes.int32_t
let int64_t = Primitive RawTypes.int64_t
let camlint = Primitive RawTypes.camlint
let uchar = Primitive RawTypes.uchar
let uint8_t = Primitive RawTypes.uint8_t
let uint16_t = Primitive RawTypes.uint16_t
let uint32_t = Primitive RawTypes.uint32_t
let uint64_t = Primitive RawTypes.uint64_t
let size_t = Primitive RawTypes.size_t
let ushort = Primitive RawTypes.ushort
let uint = Primitive RawTypes.uint
let ulong = Primitive RawTypes.ulong
let ullong = Primitive RawTypes.ullong
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

let aligned_offset offset alignment =
  match offset mod alignment with
    0 -> offset
  | overhang -> offset - overhang + alignment

let structure tag =
  Struct { spec = Incomplete { isize = 0 }; tag; fields = [] }

let union utag = Union { utag; uspec = None; ufields = [] }

let max_field_alignment fields =
  List.fold_left
    (fun align (BoxedField {ftype}) -> max align (alignment ftype))
    0
    fields

let max_field_size fields =
  List.fold_left
    (fun size (BoxedField {ftype}) -> max size (sizeof ftype))
    0
    fields

let all_passable fields =
  List.fold_left
    (fun pass (BoxedField {ftype; foffset}) -> (pass && passable ftype))
    true
    fields

let fold_boxed_field_list (o : < f : 't. 'a -> ('t, 's) field -> 'a >) a l =
  List.fold_left (fun a (BoxedField f) -> o#f a f) a l

let fold_fields (type k)
    (o : < f : 't. _ -> ('t, (_, k) structured) field -> _ >) a
    (s : (_, k) structured typ) = match s with
  | Struct { fields } -> fold_boxed_field_list o a fields
  | Union { ufields } -> fold_boxed_field_list o a ufields
  | Abstract _        -> a

let offsetof { foffset } = foffset
let field_type { ftype } = ftype
