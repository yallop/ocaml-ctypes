(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Write the dispatcher function that translates stack frame structs into
   calls. *)

val write_dispatcher : prefix:string ->
  Format.formatter -> (string * int) list -> unit
(* Write a definition of the dispatch function in C given a list of (name,
   arity) pairs. *)
