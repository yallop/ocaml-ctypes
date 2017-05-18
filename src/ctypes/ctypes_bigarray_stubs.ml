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

     In OCaml <= 4.01.0, Bigarray.char and Bigarray.int8_unsigned are
     indistinguishable, so the 'kind' function will never return Kind_char.
     OCaml 4.02.0 gives the types distinct representations. *)
  = "%identity"

external address : 'b -> Ctypes_ptr.voidp
  = "ctypes_bigarray_address"

external view : 'a kind -> dims:int array -> _ Ctypes_ptr.Fat.t ->
  'l Bigarray.layout -> ('a, 'b, 'l) Bigarray.Genarray.t
  = "ctypes_bigarray_view"

external view1 : 'a kind -> dims:int array -> _ Ctypes_ptr.Fat.t ->
  'l Bigarray.layout -> ('a, 'b, 'l) Bigarray.Array1.t
  = "ctypes_bigarray_view"

external view2 : 'a kind -> dims:int array -> _ Ctypes_ptr.Fat.t ->
  'l Bigarray.layout -> ('a, 'b, 'l) Bigarray.Array2.t
  = "ctypes_bigarray_view"

external view3 : 'a kind -> dims:int array -> _ Ctypes_ptr.Fat.t ->
  'l Bigarray.layout -> ('a, 'b, 'l) Bigarray.Array3.t
  = "ctypes_bigarray_view"
