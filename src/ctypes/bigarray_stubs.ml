(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

type _ kind =
| Ba_float32 : < element: float;
                 ba_repr: Bigarray.float32_elt;
                 storage_type: float > kind
| Ba_float64 : < element: float;
                 ba_repr: Bigarray.float64_elt;
                 storage_type: float > kind
| Ba_int8_signed : < element: int;
                     ba_repr: Bigarray.int8_signed_elt;
                     storage_type: int > kind
| Ba_int8_unsigned : < element: int;
                       ba_repr: Bigarray.int8_unsigned_elt;
                       storage_type: Unsigned.uint8 > kind
| Ba_int16_signed : < element: int;
                      ba_repr: Bigarray.int16_signed_elt;
		      storage_type: int > kind
| Ba_int16_unsigned : < element: int;
                        ba_repr: Bigarray.int16_unsigned_elt;
                        storage_type: Unsigned.uint16 > kind
| Ba_int32 : < element: int32;
               ba_repr: Bigarray.int32_elt;
               storage_type: int32 > kind
| Ba_int64 : < element: int64;
               ba_repr: Bigarray.int64_elt;
               storage_type: int64 > kind
| Ba_int : < element: int;
             ba_repr: Bigarray.int_elt;
             storage_type: int > kind
| Ba_nativeint : < element: nativeint;
                   ba_repr: Bigarray.nativeint_elt;
                   storage_type: nativeint > kind
| Ba_complex32 : < element: Complex.t;
                   ba_repr: Bigarray.complex32_elt;
                   storage_type: Complex.t > kind
| Ba_complex64 : < element: Complex.t;
                   ba_repr: Bigarray.complex64_elt;
                   storage_type: Complex.t > kind
| Ba_char : < element: char;
              ba_repr: Bigarray.int8_unsigned_elt;
              storage_type: char > kind

external kind : ('a, 'b) Bigarray.kind ->
   < element: 'a; ba_repr: 'b; storage_type: 'c > kind
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

external view : < element: 'a; .. > kind -> dims:int array ->
   Ctypes_raw.voidp -> offset:int -> ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t
  = "ctypes_bigarray_view"

external view1 : < element: 'a; .. > kind -> dims:int array ->
   Ctypes_raw.voidp -> offset:int -> ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t
  = "ctypes_bigarray_view"

external view2 : < element: 'a; .. > kind -> dims:int array ->
  Ctypes_raw.voidp -> offset:int -> ('a, 'b, Bigarray.c_layout) Bigarray.Array2.t
  = "ctypes_bigarray_view"

external view3 : < element: 'a; .. > kind -> dims:int array ->
  Ctypes_raw.voidp -> offset:int -> ('a, 'b, Bigarray.c_layout) Bigarray.Array3.t
  = "ctypes_bigarray_view"
