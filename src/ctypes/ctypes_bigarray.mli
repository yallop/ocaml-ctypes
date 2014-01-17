(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(** {2 Types *)

type ('a, 'b) t
(** The type of bigarray values of particular sizes.  A value of type
    [(a, b) t] can be used to read and write values of type [b].  *)

(** {3 Type constructors *)

val bigarray : int array -> ('a, 'b) Bigarray.kind ->
  ('a, ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t) t
(** Create a {!t} value for the {!Bigarray.Genarray.t} type. *)

val bigarray1 : int -> ('a, 'b) Bigarray.kind ->
  ('a, ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t) t
(** Create a {!t} value for the {!Bigarray.Array1.t} type. *)

val bigarray2 : int -> int -> ('a, 'b) Bigarray.kind ->
  ('a, ('a, 'b, Bigarray.c_layout) Bigarray.Array2.t) t
(** Create a {!t} value for the {!Bigarray.Array2.t} type. *)

val bigarray3 : int -> int -> int -> ('a, 'b) Bigarray.kind ->
  ('a, ('a, 'b, Bigarray.c_layout) Bigarray.Array3.t) t
(** Create a {!t} value for the {!Bigarray.Array3.t} type. *)

val prim_of_kind : ('a, _) Bigarray.kind -> 'a Primitives.prim
(** Create a {!Ctypes_raw.Types.ctype} for a {!Bigarray.kind}. *)

(** {3 Type eliminators *)

val sizeof : (_, _) t -> int
(** Compute the size of a bigarray type. *)

val alignment : (_, _) t -> int
(** Compute the alignment of a bigarray type. *)

val element_type : ('a, _) t -> 'a Primitives.prim
(** Compute the element type of a bigarray type. *)

val dimensions : (_, _) t -> int array
(** Compute the dimensions of a bigarray type. *)

val type_expression : ('a, 'b) t -> ([> `Appl of Ctypes_path.path * 'c list
                                     |  `Ident of Ctypes_path.path ] as 'c)
(** Compute a type expression that denotes a bigarray type. *)

(** {2 Values *)

val address : (_, 'a) t -> 'a -> Ctypes_raw.voidp
(** Return the address of a bigarray value. *)

val view : (_, 'a) t -> ?ref:Obj.t -> Ctypes_raw.voidp -> offset:int -> 'a
(** Create a bigarray view onto existing memory.

    The optional [ref] argument is an OCaml object that controls the lifetime
    of the memory; if [ref] is present, [view] will ensure that it is not
    collected before the bigarray returned by [view]. *)
