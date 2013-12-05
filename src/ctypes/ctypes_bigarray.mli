(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(** {2 Types *)

type ('a, 'b) t
(** The type of bigarray values of particular sizes.  A value of type
    [(a, b) t] can be used to read and write values of type [b] at particular
    addresses.  *)

type _ kind

module Kinds : sig
  val ba_float32 : < element: float;
		   ba_repr: Bigarray.float32_elt;
		   storage_type: float > kind
  val ba_float64 : < element: float;
		   ba_repr: Bigarray.float64_elt;
		   storage_type: float > kind
  val ba_int8_signed : < element: int;
		       ba_repr: Bigarray.int8_signed_elt;
		       storage_type: int > kind
  val ba_int8_unsigned : < element: int;
			 ba_repr: Bigarray.int8_unsigned_elt;
			 storage_type: Unsigned.uint8 > kind
  val ba_int16_signed : < element: int;
			ba_repr: Bigarray.int16_signed_elt;
			storage_type: int > kind
  val ba_int16_unsigned : < element: int;
			  ba_repr: Bigarray.int16_unsigned_elt;
			  storage_type: Unsigned.uint16 > kind
  val ba_int32 : < element: int32;
		 ba_repr: Bigarray.int32_elt;
		 storage_type: int32 > kind
  val ba_int64 : < element: int64;
		 ba_repr: Bigarray.int64_elt;
		 storage_type: int64 > kind
  val ba_nativeint : < element: nativeint;
		     ba_repr: Bigarray.nativeint_elt;
		     storage_type: nativeint > kind
  val ba_complex32 : < element: Complex.t;
		     ba_repr: Bigarray.complex32_elt;
		     storage_type: Complex.t > kind
  val ba_complex64 : < element: Complex.t;
		     ba_repr: Bigarray.complex64_elt;
		     storage_type: Complex.t > kind
  val ba_int : < element: int;
	       ba_repr: Bigarray.int_elt;
	       storage_type: int > kind
  val ba_char : < element: char;
		ba_repr: Bigarray.int8_unsigned_elt;
		storage_type: char > kind
end

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

val ba_kind_of_kind : < element: 'a; ba_repr: 'b; storage_type: _> kind ->
  ('a, 'b) Bigarray.kind

val storage_type_of_kind : < element: 'a; ba_repr: 'b; storage_type: 'c > kind ->
  'c Primitives.prim
(** Create a {!Ctypes_raw.Types.ctype} for a {!Bigarray.kind}. *)

(** {3 Type eliminators *)

val sizeof : (_, _) t -> int
(** Compute the size of a bigarray type. *)

val alignment : (_, _) t -> int
(** Compute the alignment of a bigarray type. *)

val format : Format.formatter -> (_, _) t -> unit
(** Pretty-print a bigarray type. *)

(** {2 Values *)

val address : (_, 'a) t -> 'a -> Ctypes_raw.voidp
(** Return the address of a bigarray value. *)

val view : (_, 'a) t -> ?ref:Obj.t -> Ctypes_raw.voidp -> offset:int -> 'a
(** Create a bigarray view onto existing memory.

    The optional [ref] argument is an OCaml object that controls the lifetime
    of the memory; if [ref] is present, [view] will ensure that it is not
    collected before the bigarray returned by [view]. *)
