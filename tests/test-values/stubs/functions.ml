(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Foreign function bindings for the OCaml value tests. *)

open Ctypes

module T = Value ()

module Custom_array = Value ()

(* These functions can be bound either dynamically using Foreign or statically
   using stub generation. *)
module Common(F : Ctypes.FOREIGN) =
struct
  let int_to_nativeint = F.(foreign "int_to_nativeint" (int @-> returning T.typ))
  let nativeint_to_int = F.(foreign "nativeint_to_int" (T.typ @-> returning int))

  let create_custom_array = F.(foreign "create_custom_array" (int @-> returning Custom_array.typ))
  let set_custom_array = F.(foreign "set_custom_array" (Custom_array.typ @-> int @-> int @-> returning void))
  let get_custom_array = F.(foreign "get_custom_array" (Custom_array.typ @-> int @-> returning int))
end
