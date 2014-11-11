(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Foreign function bindings for the coercion tests. *)

open Ctypes

module Stubs (F: Tests_common.FOREIGN) =
struct
  open F

  let memchr = foreign "memchr_wrapper"
    (ptr void @-> int @-> int @-> returning (ptr void))
end
