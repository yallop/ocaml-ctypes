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

let bigarray_kind_sizeof k = Ctypes_primitives.sizeof (prim_of_kind k)

let bigarray_kind_alignment k = Ctypes_primitives.alignment (prim_of_kind k)

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

let element_type (_, k) = prim_of_kind k

let dimensions : type a b. (b, a) t -> int array = function
| DimsGen dims, _ -> dims
| Dims1 x, _ -> [| x |]
| Dims2 (x, y), _ -> [| x; y |]
| Dims3 (x, y, z), _ -> [| x; y; z |]

let sizeof (d, k) = elements d * bigarray_kind_sizeof k

let alignment (d, k) = bigarray_kind_alignment k

let bigarray ds k = (DimsGen ds, kind k)
let bigarray1 d k = (Dims1 d, kind k)
let bigarray2 d1 d2 k = (Dims2 (d1, d2), kind k)
let bigarray3 d1 d2 d3 k = (Dims3 (d1, d2, d3), kind k)

let path_of_string = Ctypes_path.path_of_string
let type_name : type a b. (b, a) dims -> Ctypes_path.path = function
  | DimsGen _ -> path_of_string "Bigarray.Genarray.t"
  | Dims1 _ -> path_of_string "Bigarray.Array1.t"
  | Dims2 _ -> path_of_string "Bigarray.Array2.t"
  | Dims3 _ -> path_of_string "Bigarray.Array3.t"

let kind_type_names : type a. a kind -> _ = function
  | Kind_float32 ->
    (`Ident (path_of_string "float"),
     `Ident (path_of_string "Bigarray.float32_elt"))
  | Kind_float64 ->
    (`Ident (path_of_string "float"),
     `Ident (path_of_string "Bigarray.float64_elt"))
  | Kind_int8_signed ->
    (`Ident (path_of_string "int"),
     `Ident (path_of_string "Bigarray.int8_signed_elt"))
  | Kind_int8_unsigned ->
    (`Ident (path_of_string "int"),
     `Ident (path_of_string "Bigarray.int8_unsigned_elt"))
  | Kind_int16_signed ->
    (`Ident (path_of_string "int"),
     `Ident (path_of_string "Bigarray.int16_signed_elt"))
  | Kind_int16_unsigned ->
    (`Ident (path_of_string "int"),
     `Ident (path_of_string "Bigarray.int16_unsigned_elt"))
  | Kind_int32 ->
    (`Ident (path_of_string "int32"),
     `Ident (path_of_string "Bigarray.int32_elt"))
  | Kind_int64 ->
    (`Ident (path_of_string "int64"),
     `Ident (path_of_string "Bigarray.int64_elt"))
  | Kind_int ->
    (`Ident (path_of_string "int"),
     `Ident (path_of_string "Bigarray.int_elt"))
  | Kind_nativeint ->
    (`Ident (path_of_string "nativeint"),
     `Ident (path_of_string "Bigarray.nativeint_elt"))
  | Kind_complex32 ->
    (`Ident (path_of_string "Complex.t"),
     `Ident (path_of_string "Bigarray.complex32_elt"))
  | Kind_complex64 ->
    (`Ident (path_of_string "Complex.t"),
     `Ident (path_of_string "Bigarray.complex64_elt"))
  | Kind_char ->
    (`Ident (path_of_string "char"),
     `Ident (path_of_string "Bigarray.int8_unsigned_elt"))

let type_expression : type a b. (a, b) t -> _ =
  fun (t, ck) ->
  begin
    let a, b = kind_type_names ck in
    let layout = `Ident (path_of_string "Bigarray.c_layout") in
    (`Appl (type_name t, [a; b; layout])
        : [> `Ident of Ctypes_path.path
          | `Appl of Ctypes_path.path * 'a list ] as 'a)
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
