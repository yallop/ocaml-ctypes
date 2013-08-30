(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Bigarray_stubs

let prim_of_kind : type a. a kind -> a Primitives.prim
  = let open Primitives in function
    Kind_float32 -> Float
  | Kind_float64 -> Double
  | Kind_int8_signed -> Int8_t
  | Kind_int8_unsigned -> Int8_t
  | Kind_int16_signed -> Int16_t
  | Kind_int16_unsigned -> Int16_t
  | Kind_int32 -> Int32_t
  | Kind_int64 -> Int64_t
  | Kind_int -> Camlint
  | Kind_nativeint -> Nativeint
  | Kind_complex32 -> Complex32
  | Kind_complex64 -> Complex64
  | Kind_char -> Char

let string_of_kind : type a. a kind -> string
  = function
    Kind_float32 -> "float32"
  | Kind_float64 -> "float64"
  | Kind_int8_signed -> "int8_signed"
  | Kind_int8_unsigned -> "int8_unsigned"
  | Kind_int16_signed -> "int16_signed"
  | Kind_int16_unsigned -> "int16_unsigned"
  | Kind_int32 -> "int32"
  | Kind_int64 -> "int64"
  | Kind_int -> "int"
  | Kind_nativeint -> "nativeint"
  | Kind_complex32 -> "complex32"
  | Kind_complex64 -> "complex64"
  | Kind_char -> "char"

let bigarray_kind_sizeof k = Primitive_details.sizeof (prim_of_kind k)

let bigarray_kind_alignment k = Primitive_details.alignment (prim_of_kind k)

type (_, _) dims = 
| DimsGen : int array -> ('a, ('a, _, Bigarray.c_layout) Bigarray.Genarray.t) dims
| Dims1 : int -> ('a, ('a, _, Bigarray.c_layout) Bigarray.Array1.t) dims
| Dims2 : int * int -> ('a, ('a, _, Bigarray.c_layout) Bigarray.Array2.t) dims
| Dims3 : int * int * int -> ('a, ('a, _, Bigarray.c_layout) Bigarray.Array3.t) dims

type ('a, 'b) t = ('a, 'b) dims * 'a kind

let elements : type a b. (b, a) dims -> int = function
  | DimsGen ds -> Array.fold_left ( * ) 1 ds
  | Dims1 d -> d
  | Dims2 (d1, d2) -> d1 * d2
  | Dims3 (d1, d2, d3) -> d1 * d2 * d3

let sizeof (d, k) = elements d * bigarray_kind_sizeof k

let alignment (d, k) = bigarray_kind_alignment k

let bigarray ds k = (DimsGen ds, kind k)
let bigarray1 d k = (Dims1 d, kind k)
let bigarray2 d1 d2 k = (Dims2 (d1, d2), kind k)
let bigarray3 d1 d2 d3 k = (Dims3 (d1, d2, d3), kind k)

let format_kind fmt k = Format.pp_print_string fmt (string_of_kind k)

let format_dims : type a b. _ -> (b, a) dims -> unit
  = fun fmt t -> match t with
  | DimsGen ds -> Array.iter (Format.fprintf fmt "[%d]") ds
  | Dims1 d1 -> Format.fprintf fmt "[%d]" d1
  | Dims2 (d1, d2) -> Format.fprintf fmt "[%d][%d]" d1 d2
  | Dims3 (d1, d2, d3) -> Format.fprintf fmt "[%d][%d][%d]" d1 d2 d3

let format fmt (t, ck) =
  begin
    format_kind fmt ck;
    format_dims fmt t
  end

let prim_of_kind k = prim_of_kind (kind k)

let address _ b = Bigarray_stubs.address b

let view : type a b. (a, b) t -> ?ref:Obj.t -> Ctypes_raw.voidp -> offset:int -> b =
  let open Bigarray_stubs in
  fun (dims, kind) ?ref ptr ~offset -> let ba : b = match dims with
  | DimsGen ds -> view kind ds ptr offset
  | Dims1 d -> view1 kind [| d |] ptr offset
  | Dims2 (d1, d2) -> view2 kind [| d1; d2 |] ptr offset
  | Dims3 (d1, d2, d3) -> view3 kind [| d1; d2; d3 |] ptr offset in
  match ref with
  | None -> ba
  | Some src -> Gc.finalise (fun _ -> ignore src; ()) ba; ba
