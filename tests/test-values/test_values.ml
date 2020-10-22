(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open OUnit2
open Ctypes


module Common_tests(S : Cstubs.FOREIGN with type 'a result = 'a
                                        and type 'a return = 'a) =
struct
  module M = Functions.Common(S)
  open M

  (*
    Test passing values.
  *)
  let test_passing_values _ =
    let max_int = 2147483647 in
    let min_int = -2147483647 in
    begin
      assert_equal 1 (M.int_to_nativeint 1 |> M.nativeint_to_int);
      assert_equal 10000000 (M.int_to_nativeint 10000000 |> M.nativeint_to_int);
      assert_equal max_int (M.int_to_nativeint max_int |> M.nativeint_to_int);
      assert_equal min_int (M.int_to_nativeint min_int |> M.nativeint_to_int);
      assert_equal (Nativeint.of_int 1) (M.int_to_nativeint 1 |> Obj.magic);
      assert_equal (Nativeint.of_int 10000000) (M.int_to_nativeint 10000000 |> Obj.magic);
      assert_equal (Nativeint.of_int max_int) (M.int_to_nativeint max_int |> Obj.magic);
      assert_equal (Nativeint.of_int min_int) (M.int_to_nativeint min_int |> Obj.magic);
      assert_equal 1 (Nativeint.of_int 1 |> Obj.magic |> M.nativeint_to_int);
      assert_equal 10000000 (Nativeint.of_int 10000000 |> Obj.magic |> M.nativeint_to_int);
      assert_equal max_int (Nativeint.of_int max_int |> Obj.magic |> M.nativeint_to_int);
      assert_equal min_int (Nativeint.of_int min_int |> Obj.magic |> M.nativeint_to_int);
    end

  let test_array _ =
    let arr1 = M.create_custom_array 15 in
    let arr2 = M.create_custom_array 150 in
    begin
      M.set_custom_array arr1 0 0;
      M.set_custom_array arr1 1 1;
      M.set_custom_array arr1 2 100;
      M.set_custom_array arr1 3 1000;
      M.set_custom_array arr1 7 100000;
      assert_equal 0 (M.get_custom_array arr1 0);
      assert_equal 1 (M.get_custom_array arr1 1);
      assert_equal 100 (M.get_custom_array arr1 2);
      assert_equal 1000 (M.get_custom_array arr1 3);
      assert_equal 100000 (M.get_custom_array arr1 7);
      M.set_custom_array arr2 0 0;
      M.set_custom_array arr2 5 1;
      M.set_custom_array arr2 30 100;
      M.set_custom_array arr2 70 1000;
      M.set_custom_array arr2 147 100000;
      assert_equal 0 (M.get_custom_array arr2 0);
      assert_equal 1 (M.get_custom_array arr2 5);
      assert_equal 100 (M.get_custom_array arr2 30);
      assert_equal 1000 (M.get_custom_array arr2 70);
      assert_equal 100000 (M.get_custom_array arr2 147);
    end
end


module Foreign_tests = Common_tests(Tests_common.Foreign_binder)
module Stub_tests = Common_tests(Generated_bindings)


let suite = "Values tests" >:::
  ["passing values (foreign)"
   >:: Foreign_tests.test_passing_values;
   "passing larger values (foreign)"
   >:: Foreign_tests.test_array;

   "passing values (stubs)"
   >:: Stub_tests.test_passing_values;
   "passing larger values (stubs)"
   >:: Stub_tests.test_array;
  ]


let _ =
  run_test_tt_main suite
