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
module Raw = Ctypes_raw

type 'a structspec =
    Incomplete of Raw.bufferspec
  | Complete of 'a Raw.structure RawTypes.ctype

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
  | FunctionPointer : string option * ('a -> 'b) fn
                                        -> ('a -> 'b) typ
and _ fn =
  (* The flag indicates whether we should check errno *)
  | Returns  : bool * 'a typ   -> 'a fn
  | Function : 'a typ * 'b fn  -> ('a -> 'b) fn
and 'a ptr = { reftype      : 'a typ;
               raw_ptr      : Raw.raw_pointer;
               pmanaged     : Raw.managed_buffer option;
               pbyte_offset : int }
and 'a array = { astart : 'a ptr; alength : int }
and ('a, 'kind) structured = { structured : ('a, 'kind) structured ptr }
and 'a union = ('a, [`Union]) structured
and 'a structure = ('a, [`Struct]) structured
and 'a abstract = ('a, [`Abstract]) structured
and ('a, 'b) view = { read : 'b -> 'a; write : 'a -> 'b; ty: 'b typ }
and ('a, 's) field = { ftype: 'a typ; foffset: int }
and 'a structure_type = {
  tag: string;
  (* Whether the struct can be passed or returned by value.  For the
     moment, at least, we don't support passing structs that contain
     unions or arrays as members *)
  mutable passable: bool;

  mutable spec: 'a structspec;

  (* We keep the field type values around, since functions such as
     complete_struct_type may need to access the underlying C values. *)
  mutable fields : 'a structure boxed_field list;
}
and 'a union_type = {
  utag: string;
  mutable ucomplete: bool;
  mutable usize: int;
  mutable ualignment: int;
  mutable ufields : 'a union boxed_field list;
}
and 's boxed_field = BoxedField : ('a, 's) field -> 's boxed_field
type boxed_typ = BoxedType : 'a typ -> boxed_typ

type _ ccallspec =
    Call : bool * (Raw.raw_pointer -> 'a) -> 'a ccallspec
  | WriteArg : ('a -> Raw.raw_pointer -> unit) * 'b ccallspec -> ('a -> 'b) ccallspec

let rec sizeof : type a. a typ -> int = function
    Void                           -> raise IncompleteType
  | Primitive p                    -> RawTypes.sizeof p
  | Struct { spec = Incomplete _ } -> raise IncompleteType
  | Struct { spec = Complete p }   -> RawTypes.sizeof p
  | Union { ucomplete = false }    -> raise IncompleteType
  | Union { usize }                -> usize
  | Array (t, i)                   -> i * sizeof t
  | Abstract { asize }             -> asize
  | Pointer _                      -> RawTypes.(sizeof pointer)
  | FunctionPointer _              -> RawTypes.(sizeof pointer)
  | View { ty }                    -> sizeof ty

let rec alignment : type a. a typ -> int = function
    Void                           -> raise IncompleteType
  | Primitive p                    -> RawTypes.alignment p
  | Struct { spec = Incomplete _ } -> raise IncompleteType
  | Struct { spec = Complete p }   -> RawTypes.alignment p
  | Union { ucomplete = false }    -> raise IncompleteType
  | Union { ualignment }           -> ualignment
  | Array (t, _)                   -> alignment t
  | Abstract { aalignment }        -> aalignment
  | Pointer _                      -> RawTypes.(alignment pointer)
  | FunctionPointer _              -> RawTypes.(alignment pointer)
  | View { ty }                    -> alignment ty

let rec passable : type a. a typ -> bool = function
    Void                           -> true
  | Primitive p                    -> RawTypes.passable p
  | Struct { spec = Incomplete _ } -> raise IncompleteType
  | Struct { passable }            -> passable
  | Union { ucomplete = false }    -> raise IncompleteType
  | Union _                        -> false
  | Array _                        -> false
  | Pointer _                      -> true
  | Abstract _                     -> false
  | FunctionPointer _              -> true
  | View { ty }                    -> passable ty

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
let view ~read ~write ty = View { read; write; ty }
let returning v =
  if not (passable v) then
    raise (Unsupported "Unsupported return type")
  else
    Returns (false, v)
let returning_checking_errno v = Returns (true, v)
let funptr ?name f = FunctionPointer (name, f)

let structure tag =
  Struct { spec = Incomplete (Raw.allocate_bufferspec ()); tag;
           passable = true; fields = [] }

let bufferspec { tag; spec } = match spec with
  | Incomplete s -> s
  | Complete _   -> raise (ModifyingSealedType tag)

let add_field f s = s.fields <- BoxedField f :: s.fields

let ( *:* ) (type b) (Struct s) (ftype : b typ) =
    let bufspec = bufferspec s in
    let add_member ftype t =
      let foffset = Raw.add_argument bufspec t in
      add_field {ftype; foffset} s;
      foffset
    and add_unpassable_member ftype =
      let foffset = Raw.add_unpassable_argument
        bufspec ~size:(sizeof ftype) ~alignment:(alignment ftype) in
      add_field {ftype; foffset} s;
      foffset
    in
    let rec offset : type a. a typ -> int
      = fun ty -> match ty with
      | Void                           -> raise IncompleteType
      | Array _ as a                   -> (s.passable <- false;
                                           add_unpassable_member a)
      | Primitive p                    -> add_member ty p
      | Pointer _                      -> add_member ty RawTypes.pointer
      | Struct { spec = Incomplete _ } -> raise IncompleteType
      | Struct { spec = Complete t; passable }
                                       -> (s.passable <- s.passable && passable;
                                           add_member ty t)
      | Union _  as u                  -> (s.passable <- false;
                                           add_unpassable_member u)
      | Abstract _  as a               -> (s.passable <- false;
                                           add_unpassable_member a)
      | FunctionPointer _              -> add_member ty RawTypes.pointer
      | View { ty }                    -> offset ty
    in
    { ftype; foffset = offset ftype }

let union utag = Union { utag; usize = 0; ualignment = 0; ucomplete = false;
                         ufields = [] }

let ensure_unsealed { ucomplete; utag } =
  if ucomplete then raise (ModifyingSealedType utag)

let compute_union_padding { usize; ualignment } =
  let overhang = usize mod ualignment in
  if overhang = 0 then usize
  else usize - overhang + ualignment

let seal (type a) (type s) : (a, s) structured typ -> unit = function
  | Struct { fields = [] } -> raise (Unsupported "struct with no fields")
  | Struct s ->
    let bufspec = bufferspec s in
    s.spec <- Complete (Raw.complete_struct_type bufspec)
  | Union { ufields = [] } -> raise (Unsupported "union with no fields")
  | Union u -> begin
    ensure_unsealed u;
    u.usize <- compute_union_padding u;
    u.ucomplete <- true
  end

let fold_boxed_field_list (o : < f : 't. 'a -> ('t, 's) field -> 'a >) a l =
  List.fold_left (fun a (BoxedField f) -> o#f a f) a l

let fold_fields (type k)
    (o : < f : 't. _ -> ('t, (_, k) structured) field -> _ >) a
    (s : (_, k) structured typ) = match s with
  | Struct { fields } -> fold_boxed_field_list o a fields
  | Union { ufields } -> fold_boxed_field_list o a ufields
  | Abstract _        -> a

let ( +:+ ) (Union u) ftype =
  begin
    ensure_unsealed u;
    u.usize <- max u.usize (sizeof ftype);
    u.ualignment <- max u.ualignment (alignment ftype);
    let field = { ftype; foffset = 0 } in
    u.ufields <- BoxedField field :: u.ufields;
    field
  end

let offsetof { foffset } = foffset
let field_type { ftype } = ftype
