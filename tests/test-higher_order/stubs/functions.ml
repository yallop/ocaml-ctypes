(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Foreign function bindings for the higher order tests. *)

open Ctypes
open Tests_common

open Types

module Stubs (F: FOREIGN) =
struct
  open F
  let higher_order_1 = foreign "higher_order_1"
    (intfunptr @-> int @-> int @-> returning int)

  let higher_order_3 = foreign "higher_order_3"
    (funptr_acceptor @-> intfunptr @-> int @-> int @-> returning int)

  let returning_funptr = foreign "returning_funptr"
    (int @-> returning (intfunptr))

  let callback_returns_funptr = foreign "callback_returns_funptr"
    (intfunhoptr @-> int @-> returning int)
end
