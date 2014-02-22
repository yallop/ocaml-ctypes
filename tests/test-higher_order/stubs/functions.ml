(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Foreign function bindings for the higher order tests. *)

open Ctypes
open Foreign

module Stubs (F: Cstubs.FOREIGN) =
struct
  open F
  let higher_order_1 = foreign "higher_order_1"
    (funptr (int @-> int @-> returning int) @-> int @-> int @-> returning int)

  let higher_order_3 = foreign "higher_order_3"
    (funptr (funptr (int @-> int @-> returning int) @->
             int @-> int @-> returning int) @->
     funptr (int @-> int @-> returning int) @->
     int @-> int @-> returning int)

  let returning_funptr = foreign "returning_funptr"
    (int @-> returning (funptr (int @-> int @-> returning int)))

  let callback_returns_funptr = foreign "callback_returns_funptr"
    (funptr (int @-> returning (funptr (int @-> returning int))) @->
     int @-> returning int)
end
