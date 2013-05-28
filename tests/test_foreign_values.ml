open OUnit
open Ctypes


let testlib = Dl.(dlopen ~filename:"clib/test_functions.so" ~flags:[RTLD_NOW])


(*
  Retrieve a string exposed as a global value. 
*)
let test_retrieving_string () =
  let global_string = foreign_value "global_string" string ~from:testlib in
  assert_equal "global string" Ptr.(!global_string)


let suite = "Foreign value tests" >:::
  ["retrieving string" >:: test_retrieving_string;
  ]


let _ =
  run_test_tt_main suite
