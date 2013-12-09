(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

exception Uncoercible

val coerce : 'a Static.typ -> 'b Static.typ -> 'a -> 'b

val coerce_fn : 'a Static.fn -> 'b Static.fn -> 'a -> 'b
