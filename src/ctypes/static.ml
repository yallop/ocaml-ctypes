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

type _ typ =
    Void            :                       unit typ
  | Primitive       : 'a Primitives.prim -> 'a typ
  | Pointer         : 'a typ             -> 'a ptr typ
  | Struct          : 'a structure_type  -> 'a structure typ
  | View            : ('a, 'b) view      -> 'a typ
  | Array           : 'a typ * int       -> 'a carray typ
and 'a carray = { astart : 'a ptr; alength : int }
and ('a, 'kind) structured = { structured : ('a, 'kind) structured ptr }
and 'a union = ('a, [`Union]) structured
and 'a structure = ('a, [`Struct]) structured
and (_, _) pointer =
  CPointer : 'a typ Ctypes_ptr.Fat.t -> ('a, [`C]) pointer
and 'a ptr = ('a, [`C]) pointer
and ('a, 'b) view = {
  read : 'b -> 'a;
  write : 'a -> 'b;
  ty: 'b typ;
}
and ('a, 's) field = {
  ftype: 'a typ;
  foffset: int;
  fname: string;
}
and 'a structure_type = {
  tag: string;
  mutable size: int;
  mutable align: int;
  mutable complete: bool;
}

type _ fn =
  | Returns  : 'a typ   -> 'a fn
  | Function : 'a typ * 'b fn  -> ('a -> 'b) fn

type boxed_typ = BoxedType : 'a typ -> boxed_typ

let rec sizeof : type a. a typ -> int = function
    Void                           -> raise IncompleteType
  | Primitive p                    -> Ctypes_primitives.sizeof p
  | Struct { complete = false }    -> raise IncompleteType
  | Struct { size }                -> size
  | Array (t, i)                   -> i * sizeof t
  | Pointer _                      -> Ctypes_primitives.pointer_size
  | View { ty }                    -> sizeof ty

let rec alignment : type a. a typ -> int = function
    Void                             -> raise IncompleteType
  | Primitive p                      -> Ctypes_primitives.alignment p
  | Struct { complete = false }      -> raise IncompleteType
  | Struct { align }                 -> align
  | Array (t, _)                     -> alignment t
  | Pointer _                        -> Ctypes_primitives.pointer_alignment
  | View { ty }                      -> alignment ty

let rec passable : type a. a typ -> bool = function
    Void                           -> true
  | Primitive _                    -> true
  | Struct { complete = false }    -> raise IncompleteType
  | Struct _                       -> true
  | Array _                        -> false
  | Pointer _                      -> true
  | View { ty }                    -> passable ty

let void = Void
let char = Primitive Primitives.Char
let schar = Primitive Primitives.Schar
let float = Primitive Primitives.Float
let double = Primitive Primitives.Double
let short = Primitive Primitives.Short
let int = Primitive Primitives.Int
let nativeint = Primitive Primitives.Nativeint
let int8_t = Primitive Primitives.Int8_t
let int16_t = Primitive Primitives.Int16_t
let int32_t = Primitive Primitives.Int32_t
let int64_t = Primitive Primitives.Int64_t
let camlint = Primitive Primitives.Camlint
let array i t = Array (t, i)
let ptr t = Pointer t
let ( @->) f t =
  if not (passable f) then
    raise (Unsupported "Unsupported argument type")
  else
    Function (f, t)
let view ~read ~write ty = View { read; write; ty }
let returning v =
  if not (passable v) then
    raise (Unsupported "Unsupported return type")
  else
    Returns v

let structure tag =
  Struct { complete = false; size = 0; align = 0; tag }

let offsetof { foffset } = foffset
let field_type { ftype } = ftype
let field_name { fname } = fname
