(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Registry for views and structured types *)

open Static
open Ctypes_path

type paths = { value: path; typ: path }

exception Registration_error of string

let paths = Hashtbl.create 10

let id : type a. a typ -> int = function
    View { id } -> id
  | Struct { sid } -> sid
  | Union { uid } -> uid
  | Abstract { aid } -> aid
  | _ -> raise (Registration_error "not a view or structured type")

let register_paths : type a. a typ -> value:string -> typ:string -> unit =
  fun ty ~value ~typ -> Hashtbl.add paths (id ty)
    { value = path_of_string value; typ  = path_of_string typ }

let retrieve_path : type a. a typ -> paths =
  fun typ ->
    try Hashtbl.find paths (id typ)
    with Not_found -> raise (Registration_error (Ctypes.string_of_typ typ))

let () = register_paths Ctypes.string ~value:"Ctypes.string" ~typ:"string"

let ident_of_ml_prim : type a. a Primitives.ml_prim -> path =
  let open Primitives in function
   | ML_char -> path_of_string "char"
   | ML_complex -> path_of_string "Complex.t"
   | ML_float -> path_of_string "float"
   | ML_int -> path_of_string "int"
   | ML_int32 -> path_of_string "int32"
   | ML_int64 -> path_of_string "int64"
   | ML_llong -> path_of_string "Signed.llong"
   | ML_long -> path_of_string "Signed.long"
   | ML_nativeint -> path_of_string "nativeint"
   | ML_size_t -> path_of_string "Unsigned.size_t"
   | ML_uchar -> path_of_string "Unsigned.uchar"
   | ML_uint -> path_of_string "Unsigned.uint"
   | ML_uint16 -> path_of_string "Unsigned.uint16"
   | ML_uint32 -> path_of_string "Unsigned.uint32"
   | ML_uint64 -> path_of_string "Unsigned.uint64"
   | ML_uint8 -> path_of_string "Unsigned.uint8"
   | ML_ullong -> path_of_string "Unsigned.ullong"
   | ML_ulong -> path_of_string "Unsigned.ulong"
   | ML_ushort -> path_of_string "Unsigned.ushort"

let constructor_ident_of_prim : type a. a Primitives.prim -> path =
  let open Primitives in function
   | Char -> path_of_string "Ctypes.char"
   | Schar -> path_of_string "Ctypes.schar"
   | Uchar -> path_of_string "Ctypes.uchar"
   | Short -> path_of_string "Ctypes.short"
   | Int -> path_of_string "Ctypes.int"
   | Long -> path_of_string "Ctypes.long"
   | Llong -> path_of_string "Ctypes.llong"
   | Ushort -> path_of_string "Ctypes.ushort"
   | Uint -> path_of_string "Ctypes.uint"
   | Ulong -> path_of_string "Ctypes.ulong"
   | Ullong -> path_of_string "Ctypes.ullong"
   | Size_t -> path_of_string "Ctypes.size_t"
   | Int8_t -> path_of_string "Ctypes.int8_t"
   | Int16_t -> path_of_string "Ctypes.int16_t"
   | Int32_t -> path_of_string "Ctypes.int32_t"
   | Int64_t -> path_of_string "Ctypes.int64_t"
   | Uint8_t -> path_of_string "Ctypes.uint8_t"
   | Uint16_t -> path_of_string "Ctypes.uint16_t"
   | Uint32_t -> path_of_string "Ctypes.uint32_t"
   | Uint64_t -> path_of_string "Ctypes.uint64_t"
   | Camlint -> path_of_string "Ctypes.camlint"
   | Nativeint -> path_of_string "Ctypes.nativeint"
   | Float -> path_of_string "Ctypes.float"
   | Double -> path_of_string "Ctypes.double"
   | Complex32 -> path_of_string "Ctypes.complex32"
   | Complex64 -> path_of_string "Ctypes.complex64"

module ML_type= struct

  let ptr t = `Appl (path_of_string "Ctypes.ptr", [t])
  let carray t = `Appl (path_of_string "Ctypes.carray", [t])
  let structure t = `Appl (path_of_string "Ctypes.structure", [t])
  let union t = `Appl (path_of_string "Ctypes.union", [t])
  let abstract t = `Appl (path_of_string "Ctypes.abstract", [t])
  let unit = `Ident (path_of_string "unit")

  let user_type ty =
    `Ident ((retrieve_path ty).typ)

  (* This function determines the type that should appear in the
     function signature *)
  let rec typ : type a. a typ -> _ = function
   | Void -> unit
   | Primitive p -> `Ident (ident_of_ml_prim (Primitives.ml_prim p))
   | Pointer t -> ptr (typ t)
   | Array (t, _) -> carray (typ t)
   | Struct _ as t -> structure (user_type t)
   | Union _ as t -> union (user_type t)
   | Abstract _ as t -> abstract (user_type t)
   | View _ as t -> user_type t
   | Bigarray ba -> Ctypes_bigarray.type_expression ba

  let rec fn : type a. a fn -> _ = function
    | Returns t -> typ t
    | Function (f, t) -> `Fn (typ f, fn t)
end

let ident_of_fn = ML_type.fn
