(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Module [Bigarray_kind_conv] is generated at compile-time, see [configure/gen_bigarray_kind_conv].
   We re-export the generated type here. *)
type 'a kind = 'a Ctypes_bigarray_kind_conv.kind =
  | Kind_float32 : float kind
  | Kind_float64 : float kind
  | Kind_int8_signed : int kind
  | Kind_int8_unsigned : int kind
  | Kind_int16_signed : int kind
  | Kind_int16_unsigned : int kind
  | Kind_int32 : int32 kind
  | Kind_int64 : int64 kind
  | Kind_int : int kind
  | Kind_nativeint : nativeint kind
  | Kind_complex32 : Complex.t kind
  | Kind_complex64 : Complex.t kind
  | Kind_char : char kind
  | Kind_float16 : float kind

let kind = Ctypes_bigarray_kind_conv.kind

external address : 'b -> Ctypes_ptr.voidp
  = "ctypes_bigarray_address"

external view : 'a kind -> dims:int array -> _ Ctypes_ptr.Fat.t ->
  'l Bigarray_compat.layout -> ('a, 'b, 'l) Bigarray_compat.Genarray.t
  = "ctypes_bigarray_view"

external view1 : 'a kind -> dims:int array -> _ Ctypes_ptr.Fat.t ->
  'l Bigarray_compat.layout -> ('a, 'b, 'l) Bigarray_compat.Array1.t
  = "ctypes_bigarray_view"

external view2 : 'a kind -> dims:int array -> _ Ctypes_ptr.Fat.t ->
  'l Bigarray_compat.layout -> ('a, 'b, 'l) Bigarray_compat.Array2.t
  = "ctypes_bigarray_view"

external view3 : 'a kind -> dims:int array -> _ Ctypes_ptr.Fat.t ->
  'l Bigarray_compat.layout -> ('a, 'b, 'l) Bigarray_compat.Array3.t
  = "ctypes_bigarray_view"
