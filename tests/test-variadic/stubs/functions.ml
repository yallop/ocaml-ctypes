(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Foreign function bindings for the variadic function tests. *)

open Ctypes

module Stubs (F: Tests_common.FOREIGN) =
struct
  open F

  let bind_snprintf tail =
    foreign "snprintf_wrapper" (ptr char @-> int @-> string @-> tail)

  let snprintf_int =
    bind_snprintf (int @-> returning int)

  let snprintf_char_float =
    bind_snprintf (char @-> float @-> returning int)

  let snprintf_string_short =
    bind_snprintf (string @-> short @-> returning int)
end
