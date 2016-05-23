(*
 * Copyright (c) 2016 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open OUnit2
open Ctypes


module Bindings = Functions.Stubs(Generated_bindings)

(*
  Test the Lwt binding to "sqrt".
 *)
let test_sqrt _ =
  Lwt_unix.run
    Lwt.((Bindings.sqrt 9.0).Generated_bindings.lwt >>= fun x ->
         return (assert (x = 3.0)))


let suite = "Lwt job tests" >:::
  ["calling sqrt"
    >:: test_sqrt;
  ]



let _ =
  run_test_tt_main suite
