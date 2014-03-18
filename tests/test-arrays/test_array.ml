(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open OUnit
open Ctypes


let testlib = Dl.(dlopen ~filename:"clib/libtest_functions.so" ~flags:[RTLD_NOW])


(*
  Creating multidimensional arrays, and reading and writing elements.
*)
let test_multidimensional_arrays () =
  let module Array = CArray in
  (* one dimension *)
  let one = Array.make int 10 in
  
  for i = 0 to Array.length one - 1 do
    one.(i) <- i
  done;

  for i = 0 to Array.length one - 1 do
    assert_equal i one.(i)
  done;

  (* two dimensions *)
  let two = Array.make (array 5 char) 10 in
  let s = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" in

  for i = 0 to 9 do
    for j = 0 to 4 do
      two.(i).(j) <- s.[i + j]
    done
  done;

  for i = 0 to 9 do
    for j = 0 to 4 do
      assert_equal two.(i).(j) s.[i + j]
        ~printer:(String.make 1)
    done
  done;

  (* three dimensions *)
  let three = Array.make (array 2 (array 5 float)) 10 in
  let float = Pervasives.float in

  for i = 0 to 9 do
    for j = 0 to 1 do
      for k = 0 to 4 do
      three.(i).(j).(k) <- float i *. float j -. float k
      done
    done
  done;

  for i = 0 to 9 do
    for j = 0 to 1 do
      for k = 0 to 4 do
        assert_equal three.(i).(j).(k) (float i *. float j -. float k)
          ~printer:string_of_float
      done
    done
  done;

  (* four *)
  let four = Array.make (array 3 (array 2 (array 5 int32_t))) 10 in

  for i = 0 to 9 do
    for j = 0 to 2 do
      for k = 0 to 1 do
        for l = 0 to 4 do
          four.(i).(j).(k).(l)
          <- Int32.(mul (sub (of_int i) (of_int j)) (add (of_int k) (of_int l)))
        done
      done
    done
  done;

  for i = 0 to 9 do
    for j = 0 to 2 do
      for k = 0 to 1 do
        for l = 0 to 4 do
          assert_equal four.(i).(j).(k).(l)
          Int32.(mul (sub (of_int i) (of_int j)) (add (of_int k) (of_int l)))
            ~printer:Int32.to_string
        done
      done
    done
  done


(*
  Test that creating an array initializes all elements appropriately.
*)
let test_array_initialiation () =
  let module Array = CArray in
  let int_array = Array.make int ~initial:33 10 in
  for i = 0 to Array.length int_array - 1 do
    assert_equal 33 int_array.(i)
  done;

  let int_array_array = Array.make (array 10 int) ~initial:int_array 5 in
  for i = 0 to Array.length int_array_array - 1 do
    for j = 0 to Array.length int_array_array.(i) - 1 do
      assert_equal 33 int_array_array.(i).(j)
    done
  done


(*
  Test that creating an array initializes all elements appropriately.
*)
let test_pointer_to_array_arithmetic () =
  let module Array = CArray in
  (* int ( * )[3] *)
  let p = allocate_n (array 3 int) ~count:4 in
  p <-@ Array.of_list int [1; 2; 3];
  (p +@ 1) <-@ Array.of_list int [4; 5; 6];
  (p +@ 2) <-@ Array.of_list int [7; 8; 9];
  (p +@ 3) <-@ Array.of_list int [10; 11; 12];
  let q = p in
  assert_equal 8 (!@(q +@ 2)).(1);
  assert_equal 12 (!@(q +@ 3)).(2);
  assert_equal 1 (!@(q +@ 0)).(0);
  let a = Array.from_ptr p 4 in
  assert_equal 8 a.(2).(1);
  assert_equal 12 a.(3).(2);
  assert_equal 1 a.(0).(0)

module Common_tests(S : Cstubs.FOREIGN with type 'a fn = 'a) =
struct
  module M = Functions.Stubs(S)
  open M

  (*
    Test passing pointer to array of structs.
  *)
  let test_passing_pointer_to_array_of_structs () =
    let box_int x =
      let v = make s in
      setf v tag 'i';
      let pd = v @. data in
      (pd |-> i) <-@ x;
      v
    in

    let box_double x =
      let v = make s in
      setf v tag 'd';
      let pd = v @. data in
      (pd |-> d) <-@ x;
      v
    in

    let sum = 
      accepts_pointer_to_array_of_structs
        (from_voidp
           (array 5 s)
           (to_voidp
              (CArray.start
                 (CArray.of_list s
                    [box_int 10;
                     box_double 3.5;
                     box_int 12;
                     box_double (-14.1);
                     box_double (103.25)]))))
    in
    assert_equal
      (103.25 +. (-14.1) +. 12.0 +. 3.5 +. 10.0)
      sum
end

module Foreign_tests = Common_tests(Tests_common.Foreign_binder)
module Stub_tests = Common_tests(Generated_bindings)

let suite = "Array tests" >:::
  ["multidimensional arrays"
    >:: test_multidimensional_arrays;

   "array initialization"
    >:: test_array_initialiation;

   "pointer to array arithmetic"
    >:: test_pointer_to_array_arithmetic;

   "passing pointer to array of structs (foreign)"
    >:: Foreign_tests.test_passing_pointer_to_array_of_structs;

   "passing pointer to array of structs (stubs)"
    >:: Stub_tests.test_passing_pointer_to_array_of_structs;
  ]


let _ =
  run_test_tt_main suite
