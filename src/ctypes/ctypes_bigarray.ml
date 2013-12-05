(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Bigarray_stubs

type 'a kind = 'a Bigarray_stubs.kind

module Kinds = struct
  let ba_float32 = Ba_float32
  let ba_float64 = Ba_float64
  let ba_int8_signed = Ba_int8_signed
  let ba_int8_unsigned = Ba_int8_unsigned
  let ba_int16_signed = Ba_int16_signed
  let ba_int16_unsigned = Ba_int16_unsigned
  let ba_int32 = Ba_int32
  let ba_int64 = Ba_int64
  let ba_nativeint = Ba_nativeint
  let ba_complex32 = Ba_complex32
  let ba_complex64 = Ba_complex64
  let ba_int = Ba_int
  let ba_char = Ba_char
end

let storage_type_of_kind : type a b c.
   < element: a; ba_repr: b; storage_type: c > kind -> c Primitives.prim
  = let open Primitives in function
    Ba_float32 -> Float
  | Ba_float64 -> Double
  | Ba_int8_signed -> Int8_t
  | Ba_int8_unsigned -> Uint8_t
  | Ba_int16_signed -> Int16_t
  | Ba_int16_unsigned -> Uint16_t
  | Ba_int32 -> Int32_t
  | Ba_int64 -> Int64_t
  | Ba_int -> Camlint
  | Ba_nativeint -> Nativeint
  | Ba_complex32 -> Complex32
  | Ba_complex64 -> Complex64
  | Ba_char -> Char

let ba_kind_of_kind : type a b c. 
  < element: a; ba_repr: b; storage_type: c> kind -> (a, b) Bigarray.kind =
  let open Bigarray in function
    Ba_float32 -> float32
  | Ba_float64 -> float64
  | Ba_int8_signed -> int8_signed
  | Ba_int8_unsigned -> int8_unsigned
  | Ba_int16_signed -> int16_signed
  | Ba_int16_unsigned -> int16_unsigned
  | Ba_int32 -> int32
  | Ba_int64 -> int64
  | Ba_int -> int
  | Ba_nativeint -> nativeint
  | Ba_complex32 -> complex32
  | Ba_complex64 -> complex64
  | Ba_char -> char

let string_of_kind : type a. a kind -> string
  = function
    Ba_float32 -> "float32"
  | Ba_float64 -> "float64"
  | Ba_int8_signed -> "int8_signed"
  | Ba_int8_unsigned -> "int8_unsigned"
  | Ba_int16_signed -> "int16_signed"
  | Ba_int16_unsigned -> "int16_unsigned"
  | Ba_int32 -> "int32"
  | Ba_int64 -> "int64"
  | Ba_int -> "int"
  | Ba_nativeint -> "nativeint"
  | Ba_complex32 -> "complex32"
  | Ba_complex64 -> "complex64"
  | Ba_char -> "char"

let bigarray_kind_sizeof k = Ctypes_primitives.sizeof (storage_type_of_kind k)

let bigarray_kind_alignment k = Ctypes_primitives.alignment (storage_type_of_kind k)

type (_, _) dims = 
| DimsGen : int array -> ('a, ('a, _, Bigarray.c_layout) Bigarray.Genarray.t) dims
| Dims1 : int -> ('a, ('a, _, Bigarray.c_layout) Bigarray.Array1.t) dims
| Dims2 : int * int -> ('a, ('a, _, Bigarray.c_layout) Bigarray.Array2.t) dims
| Dims3 : int * int * int -> ('a, ('a, _, Bigarray.c_layout) Bigarray.Array3.t) dims

type ('a, 'b) t =
  T : ('a, 'b) dims * < element: 'a; ba_repr: _; storage_type: _ > kind -> ('a, 'b) t

let elements : type a b. (b, a) dims -> int = function
  | DimsGen ds -> Array.fold_left ( * ) 1 ds
  | Dims1 d -> d
  | Dims2 (d1, d2) -> d1 * d2
  | Dims3 (d1, d2, d3) -> d1 * d2 * d3

let sizeof (T (d, k)) = elements d * bigarray_kind_sizeof k

let alignment (T (d, k)) = bigarray_kind_alignment k

let bigarray ds k = T (DimsGen ds, kind k)
let bigarray1 d k = T (Dims1 d, kind k)
let bigarray2 d1 d2 k = T (Dims2 (d1, d2), kind k)
let bigarray3 d1 d2 d3 k = T (Dims3 (d1, d2, d3), kind k)

let format_kind fmt k = Format.pp_print_string fmt (string_of_kind k)

let format_dims : type a b. _ -> (b, a) dims -> unit
  = fun fmt t -> match t with
  | DimsGen ds -> Array.iter (Format.fprintf fmt "[%d]") ds
  | Dims1 d1 -> Format.fprintf fmt "[%d]" d1
  | Dims2 (d1, d2) -> Format.fprintf fmt "[%d][%d]" d1 d2
  | Dims3 (d1, d2, d3) -> Format.fprintf fmt "[%d][%d][%d]" d1 d2 d3

let format fmt (T (t, ck)) =
  begin
    format_kind fmt ck;
    format_dims fmt t
  end

let address _ b = Bigarray_stubs.address b

let view : type a b. (a, b) t -> ?ref:Obj.t -> Ctypes_raw.voidp -> offset:int -> b =
  let open Bigarray_stubs in
  fun (T (dims, kind)) ?ref ptr ~offset -> let ba : b = match dims with
  | DimsGen ds -> view kind ds ptr offset
  | Dims1 d -> view1 kind [| d |] ptr offset
  | Dims2 (d1, d2) -> view2 kind [| d1; d2 |] ptr offset
  | Dims3 (d1, d2, d3) -> view3 kind [| d1; d2; d3 |] ptr offset in
  match ref with
  | None -> ba
  | Some src -> Gc.finalise (fun _ -> ignore src; ()) ba; ba
