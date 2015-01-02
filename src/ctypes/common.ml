(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Backport from 4.01 *)
let asprintf fmt =
  let b = Buffer.create 512 in
  let ppf = Format.formatter_of_buffer b in
  let k ppf =
    Format.pp_print_flush ppf ();
    Buffer.contents b
  in
  Format.kfprintf k ppf fmt

let string_of format v = asprintf "%a" format v
