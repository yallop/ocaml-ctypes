(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

val record : Obj.t -> Obj.t -> int
(** [record c v] links the lifetimes of [c] and [v], ensuring that [v] is not
    collected while [c] is still live.  The return value is a key
    that can be used to retrieve [v] while [v] is still live. *)

val retrieve : int -> Obj.t
(** [retrieve v] retrieves a value using a key returned by [record], or raises
    [Not_found] if [v] is no longer live. *)
