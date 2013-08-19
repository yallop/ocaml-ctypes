(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Static

(** Dynamic function calls based on libffi *)

val function_of_pointer : ?name:string -> ('a -> 'b) fn -> unit ptr -> ('a -> 'b)
(** Build an OCaml function from a type specification and a pointer to a C
    function. *)

val pointer_of_function : ('a -> 'b) fn -> ('a -> 'b) -> unit ptr
(** Build an C function from a type specification and an OCaml function.

    The C function pointer returned is callable as long as the OCaml function
    value is live. *)
