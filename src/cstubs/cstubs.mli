(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Cstubs public interface. *)

val write_c : cname:string -> stub_name:string ->
  Format.formatter -> ('a -> 'b) Ctypes.fn -> unit
(** [write_c ~stub_name ~cname fmt fn] writes a C stub with name [stub_name]
    for the C function [cname] with type [fn] to the formatter [fmt].

    The generated code uses definitions exposed in the header file
    [cstubs_internals.h].
*)

val write_ml : stub_name:string -> external_name:string ->
  Format.formatter -> ('a -> 'b) Ctypes.fn -> unit
(** [write_ml ~stub_name ~external_name fmt fn] writes an 'external'
    declaration and, if necessary, a wrapper function with identifier
    [external_name] for the C stub with name [stub_name] to the formatter
    [fmt].

    The generated code uses definitions exposed in the module
    [Cstubs_internals].
*)

val write_signature : string ->
  Format.formatter -> ('a -> 'b) Ctypes.fn -> unit
(** [write_signature name fmt fn] writes an OCaml signature for the function
    [name] with type [fn] to the formatter [fmt].
*)

val register_paths : 'a Ctypes.typ -> value:string -> typ:string -> unit
(** [register_paths typ ~value ~typ] records paths for the view or structured
    type representation [typ].
*)
