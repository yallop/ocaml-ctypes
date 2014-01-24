(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Foreign function bindings for the views tests. *)

open Ctypes
open Tests_common

open Types

module Stubs (F: FOREIGN) =
struct
  open F

  let concat_strings = foreign "concat_strings"
    (ptr string @-> int @-> ptr char @-> returning void)

  let toupper = foreign "toupper"
    (charish @-> returning charish)

  let returning_funptr = foreign "returning_funptr"
    (int @-> returning nullable_intptr)
  let accepting_possibly_null_funptr = foreign "accepting_possibly_null_funptr"
    (nullable_intptr @-> int @-> int @-> returning int)
end
