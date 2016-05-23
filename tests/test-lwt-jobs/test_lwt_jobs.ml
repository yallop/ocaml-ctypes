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

(*
  Test that objects remain alive during the Lwt job call.
 *)
let test_object_lifetime _ =
  let call = 
    let open Bigarray in 
    let b = Array1.create int32 c_layout 3 in
    begin
      b.{0} <- 1l;
      b.{1} <- 2l;
      b.{2} <- 3l;
    end;
    (Bindings.sum_int_array
       (bigarray_start array1 b)
       (Unsigned.Size_t.of_int 3)).Generated_bindings.lwt
  in
  begin
    Gc.compact ();
    Gc.compact ();
    Lwt_unix.run
      (Lwt.(call >>= fun n ->
            assert_equal 6l n ~printer:Int32.to_string;
            return ()))
  end


let suite = "Lwt job tests" >:::
  ["calling sqrt"
    >:: test_sqrt;

   "object lifetime"
    >:: test_object_lifetime;
  ]



let _ =
  run_test_tt_main suite
