(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* C stub generation *)

val fn : cname:string -> stub_name:string -> Format.formatter ->
         'a Ctypes.fn -> unit
