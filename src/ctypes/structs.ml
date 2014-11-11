(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Static

module type S =
sig
  type (_, _) field
  val field : 's structure typ -> string -> 'a typ -> ('a, 's structure) field
  val seal : _ structure typ -> unit
end
