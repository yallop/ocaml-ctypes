(*
 * Copyright (c) 2016 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open OUnit2
open Ctypes
open Unsigned


(*
  Test UInt64.of_int.
*)
let test_uint64_of_int _ =
  begin
    assert_equal max_int (UInt64.to_int (UInt64.of_int max_int))
  end


let suite = "Integer tests" >:::
  ["UInt64.of_int"
    >:: test_uint64_of_int;
  ]



let _ =
  run_test_tt_main suite
