(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* C type construction.  Internal representation, not for public use. *)

type _ typ =
    Void            :                       unit typ
  | Primitive       : 'a Primitives.prim -> 'a typ
  | Pointer         : 'a typ             -> 'a ptr typ
  | Struct          : 'a structure_type  -> 'a structure typ
  | View            : ('a, 'b) view      -> 'a typ
and 'a structure = { structure : 'a structure ptr }
and 'a ptr = CPointer of 'a typ Ctypes_ptr.Fat.t
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

val sizeof : 'a typ -> int
val alignment : 'a typ -> int
val passable : 'a typ -> bool

val void : unit typ
val char : char typ
val schar : int typ
val float : float typ
val double : float typ
val short : int typ
val int : int typ
val nativeint : nativeint typ
val int8_t : int typ
val int16_t : int typ
val int32_t : int32 typ
val int64_t : int64 typ
val camlint : int typ
val ptr : 'a typ -> 'a ptr typ
val ( @-> ) : 'a typ -> 'b fn -> ('a -> 'b) fn
val view : read:('a -> 'b) -> write:('b -> 'a) -> 'a typ -> 'b typ
val returning : 'a typ -> 'a fn
val structure : string -> 'a structure typ
val offsetof : ('a, 'b) field -> int
val field_type : ('a, 'b) field -> 'a typ
val field_name : ('a, 'b) field -> string

exception IncompleteType
exception ModifyingSealedType of string
exception Unsupported of string

val unsupported : ('a, unit, string, _) format4 -> 'a
