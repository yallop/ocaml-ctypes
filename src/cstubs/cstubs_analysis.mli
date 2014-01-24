(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Analysis for stub generation *)

val float : 'a Static.fn -> bool
val may_allocate : 'a Static.fn -> bool
