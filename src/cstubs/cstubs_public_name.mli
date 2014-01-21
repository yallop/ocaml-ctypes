(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Registry for views and structured types *)

open Ctypes_path

type paths = { value: path; typ: path }

val register_paths : 'a Static.typ -> value:string -> typ:string -> unit
val retrieve_path : 'a Static.typ -> paths

exception Registration_error of string

val ident_of_ml_prim : 'a Primitives.ml_prim -> path
(* The type that should appear in the extern signature *)

val constructor_ident_of_prim : 'a Primitives.prim -> path
(* The path to a value that represents the primitive type *)

val ident_of_fn : 'a Static.fn ->
  ([ `Ident of path
   | `Appl of path * 'ml_type list
   | `Fn of 'ml_type * 'ml_type ] as 'ml_type)
