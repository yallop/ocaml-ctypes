(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Foreign function bindings for the OCaml-value-passing tests. *)

open Ctypes

let name_strdup =
  match Sys.os_type with
    | "Win32" -> "_strdup"
    | _ -> "strdup"

module Stubs (F: Ctypes.FOREIGN) =
struct
  open F

  let memcpy_string_string = foreign "memcpy"
    (ocaml_string @-> ocaml_string @-> size_t @-> returning (ptr void))

  let memcpy_bytes_bytes = foreign "memcpy"
    (ocaml_bytes @-> ocaml_bytes @-> size_t @-> returning (ptr void))

  let memcpy_string_ptr = foreign "memcpy"
    (ocaml_string @-> ptr void @-> size_t @-> returning (ptr void))

  let strdup = foreign name_strdup
    (ocaml_string @-> returning string)

  let int_ref_ref_typ : int ref ref typ =
     ocaml_value  "int ref ref"
          ~format:(fun fmt (x:int ref ref) ->
                       Format.fprintf fmt "ref (ref %i)" !(!x))

  let int_ref_typ : int ref typ =
     ocaml_value  "int ref"
          ~format:(fun fmt (x:int ref) ->
                       Format.fprintf fmt "(ref %i)" !x)

  let get_first_field_int_ref_ref = foreign "get_first_field"
    (int_ref_ref_typ @-> returning int_ref_typ)

  let get_first_field_obj_t = foreign "get_first_field"
      (ocaml_obj_t @-> returning ocaml_obj_t)


end
