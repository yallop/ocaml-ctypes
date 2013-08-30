(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

type _ kind =
    Kind_float32 : float kind
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

external kind : ('a, 'b) Bigarray.kind -> 'a kind
  (* Bigarray.kind is simply an int whose values are consecutively numbered
     starting from zero, so we can directly transform its values to a variant
     with appropriately-ordered constructors.

     Unfortunately, Bigarray.char and Bigarray.int8_unsigned are currently
     indistinguishable, so the 'kind' function will never return Kind_char.
     Mantis bug 6064 has a patch that gives char and int8_unsigned distinct
     representations. *)
  = "%identity"

external address : 'b -> Ctypes_raw.voidp
  = "ctypes_bigarray_address"

external view : 'a kind -> dims:int array -> Ctypes_raw.voidp -> offset:int ->
  ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t
  = "ctypes_bigarray_view"

external view1 : 'a kind -> dims:int array -> Ctypes_raw.voidp -> offset:int ->
  ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t
  = "ctypes_bigarray_view"

external view2 : 'a kind -> dims:int array -> Ctypes_raw.voidp -> offset:int ->
  ('a, 'b, Bigarray.c_layout) Bigarray.Array2.t
  = "ctypes_bigarray_view"

external view3 : 'a kind -> dims:int array -> Ctypes_raw.voidp -> offset:int ->
  ('a, 'b, Bigarray.c_layout) Bigarray.Array3.t
  = "ctypes_bigarray_view"
