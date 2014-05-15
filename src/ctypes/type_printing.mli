(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Static

val string_of_typ : ?name:string -> 'a typ -> string

val string_of_fn : ?name:string -> 'a fn -> string

val format_typ : ?name:string -> Format.formatter -> 'a typ -> unit

val format_fn' : 'a fn -> (Format.formatter -> unit) -> Format.formatter -> unit

val format_fn : ?name:string -> Format.formatter -> 'a fn -> unit
