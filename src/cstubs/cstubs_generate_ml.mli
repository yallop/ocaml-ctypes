(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* ML stub generation *)

val fn : stub_name:string -> external_name:string -> Format.formatter ->
         ('a -> 'b) Ctypes.fn -> unit

val case : stub_name:string -> external_name:string -> Format.formatter ->
         ('a -> 'b) Ctypes.fn -> unit
