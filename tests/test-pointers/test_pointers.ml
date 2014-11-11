(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open OUnit2
open Ctypes
open Foreign


let testlib = Dl.(dlopen ~filename:"clib/libtest_functions.so" ~flags:[RTLD_NOW])

module Common_tests(S : Tests_common.FOREIGN with type 'a fn = 'a) =
struct
  module M = Functions.Stubs(S)
  open M

  (*
    Test passing various types of pointers to a function.
  *)
  let test_passing_pointers _ =
    assert_equal ~msg:"Passing pointers to various numeric types"
      ~printer:string_of_int
      (1 + 2 + 3 + 4 + 7 + 8 + 9 + 10 + 11)
      (accept_pointers
         (allocate float 1.0)
         (allocate double 2.0)
         (allocate short 3)
         (allocate int 4)
         (allocate nativeint 7n)
         (allocate int8_t 8)
         (allocate int16_t 9)
         (allocate int32_t 10l)
         (allocate int64_t 11L))

  (*
    Test passing pointers to pointers.
  *)
  let test_passing_pointers_to_pointers _ =
    let p = allocate int 1
    and pp = allocate (ptr int) (allocate int 2)
    and ppp = allocate (ptr (ptr int)) (allocate (ptr int) (allocate int 3))
    and pppp = allocate (ptr (ptr (ptr int)))
      (allocate (ptr (ptr int)) (allocate (ptr int) (allocate int 4))) in

    assert_equal ~msg:"Passing pointers to pointers"
      Pervasives.(1 + 2 + 3 + 4)
      (accept_pointers_to_pointers p pp ppp pppp)


  (*
    Passing a callback that accepts pointers as arguments.
  *)
  let test_callback_receiving_pointers _ =
    assert_equal 7
      (passing_pointers_to_callback (fun lp rp -> !@lp + !@rp))


  (*
    Passing a callback that returns a pointer.
  *)
  let test_callback_returning_pointers _ =
    let p = allocate int 17 in
    begin
      assert_equal 17 !@p;

      assert_equal 56
        (accepting_pointer_from_callback (fun x y -> p <-@ (x * y); p));

      assert_equal 12 !@p
    end


  (*
    Test passing a pointer-to-a-function-pointer as an argument.
  *)
  let test_passing_pointer_to_function_pointer _ =
    assert_equal ~printer:string_of_int
      5 (accepting_pointer_to_function_pointer 
           (allocate (funptr (int @-> int @-> returning int)) ( / )))



  (*
    Test returning a pointer to a function pointer
  *)
  let test_callback_returning_pointer_to_function_pointer _ =
    assert_equal
      10 (!@(returning_pointer_to_function_pointer ()) 2 5)


  (*
    Test a function that returns the address of a global variable.
  *)
  let test_reading_returned_global _ =
    assert_equal (!@(return_global_address ())) 100


  (*
    Test a function that returns a pointer passed as argument.
  *)
  let test_passing_pointer_through _ =
    let p1 = allocate int 25 in
    let p2 = allocate int 32 in
    let rv = pass_pointer_through p1 p2 10 in
    assert_equal !@rv !@p1;
    assert_equal 25 !@rv;
    let rv = pass_pointer_through p1 p2 (-10) in
    assert_equal !@rv !@p2;
    assert_equal 32 !@rv;
    let p3 = p1 +@ 1 in
    let rv = pass_pointer_through p3 p1 1 in
    assert_bool
      "pointer with (positive) offset successfully passed through"
      (ptr_compare rv p3 = 0);
    assert_bool
      "pointer with positive computed offset compares greater than original"
      (ptr_compare p1 p3 < 0);
    assert_bool
      "pointer with positive computed offset compares greater than original"
      (ptr_compare p3 p1 > 0);
    assert_bool
      "returned pointer with positive computed offset compares greater than original"
      (ptr_compare p1 rv < 0);
    assert_bool
      "returned pointer with positive computed offset compares greater than original"
      (ptr_compare rv p1 > 0);
    assert_equal !@(rv -@ 1) !@(p3 -@ 1);
    let p4 = p1 -@ 1 in
    let rv = pass_pointer_through p1 p4 (-1) in
    assert_bool
      "pointer with (negative) offset successfully passed through"
      (ptr_compare rv p4 = 0);
    assert_bool
      "pointer with negative computed offset compares less than original"
      (ptr_compare p1 p4 > 0);
    assert_bool
      "pointer with negative computed offset compares less than original"
      (ptr_compare p4 p1 < 0);
    assert_bool
      "returned pointer with negative computed offset compares greater than original"
      (ptr_compare p1 rv > 0);
    assert_bool
      "returned pointer with negative computed offset compares greater than original"
      (ptr_compare rv p1 < 0)
end


(*
  Tests for reading and writing primitive values through pointers.
*)
let test_pointer_assignment_with_primitives _ =
  let p_char = allocate char '1'
  and p_schar = allocate schar 3
  and p_float = allocate float 4.0
  and p_double = allocate double 5.0
  and p_short = allocate short 6
  and p_int = allocate int 7
  and p_nativeint = allocate nativeint 10n
  and p_int8_t = allocate int8_t 11
  and p_int16_t = allocate int16_t 12
  and p_int32_t = allocate int32_t 13l
  and p_int64_t = allocate int64_t 14L
  in begin
    assert_equal '1' (!@p_char);
    assert_equal 3 (!@p_schar);
    assert_equal 4.0 (!@p_float);
    assert_equal 5.0 (!@p_double);
    assert_equal 6 (!@p_short);
    assert_equal 7 (!@p_int);
    assert_equal 10n (!@p_nativeint);
    assert_equal 11 (!@p_int8_t);
    assert_equal 12 (!@p_int16_t);
    assert_equal 13l (!@p_int32_t);
    assert_equal 14L (!@p_int64_t);

    p_char <-@ '2';
    p_schar <-@ 103;
    p_float <-@ 104.0;
    p_double <-@ 105.0;
    p_short <-@ 106;
    p_int <-@ 107;
    p_nativeint <-@ 110n;
    p_int8_t <-@ 111;
    p_int16_t <-@ 112;
    p_int32_t <-@ 113l;
    p_int64_t <-@ 114L;

    assert_equal '2' (!@p_char);
    assert_equal 103 (!@p_schar);
    assert_equal 104.0 (!@p_float);
    assert_equal 105.0 (!@p_double);
    assert_equal 106 (!@p_short);
    assert_equal 107 (!@p_int);
    assert_equal 110n (!@p_nativeint);
    assert_equal 111 (!@p_int8_t);
    assert_equal 112 (!@p_int16_t);
    assert_equal 113l (!@p_int32_t);
    assert_equal 114L (!@p_int64_t);
  end


(*
  Dereferencing pointers to incomplete types
*)
let test_dereferencing_pointers_to_incomplete_types _ =
  begin
    assert_raises IncompleteType
      (fun () -> !@null);

    assert_raises IncompleteType
      (fun () -> !@(from_voidp (structure "incomplete") null));
  end


(* 
   Test for reading and writing global values using the "foreign_value"
   function.
*)
let test_reading_and_writing_global_value _ =
  let ptr = foreign_value "global" int ~from:testlib in
  let ptr' = foreign_value "global" int ~from:testlib in
  assert_equal (!@ptr) 100;
  ptr <-@ 200;
  assert_equal (!@ptr) 200;
  assert_equal (!@ptr') 200;
  ptr' <-@ 100;
  assert_equal (!@ptr) 100;
  assert_equal (!@ptr') 100


(*
  Tests for reading a string from an address.
*)
let test_reading_strings _ =
  let p = allocate_n char 26 in begin
    StringLabels.iteri "abcdefghijklmnoprwstuvwxyz"
      ~f:(fun i c -> (p +@ i) <-@ c);
    assert_equal (string_from_ptr p 5) "abcde";
    assert_equal (string_from_ptr p 26) "abcdefghijklmnoprwstuvwxyz";
    assert_equal (string_from_ptr p 0) "";
    assert_raises (Invalid_argument "Ctypes.string_from_ptr")
      (fun () -> string_from_ptr p (-1));
  end


(*
  Tests for various aspects of pointer arithmetic.
*)
let test_pointer_arithmetic _ =
  let module Array = CArray in
  let arr = Array.of_list int [1;2;3;4;5;6;7;8] in

  (* Traverse the array using an int pointer *)
  let p = Array.start arr in
  for i = 0 to 7 do
    assert_equal !@(p +@ i) (succ i)
  done;

  let twoints = structure "s" in
  let i1 = field twoints "i" int in
  let i2 = field twoints "j" int in
  let () = seal twoints in

  (* Traverse the array using a 'struct twoints' pointer *)
  let ps = from_voidp twoints (to_voidp p) in

  for i = 0 to 3 do
    assert_equal !@((ps +@ i) |-> i1) (2 * i + 1);
    assert_equal !@((ps +@ i) |-> i2) (2 * i + 2);
  done;

  (* Traverse the array using a char pointer *)
  let pc = from_voidp char (to_voidp p) in

  for i = 0 to 7 do
    let p' = pc +@ i * sizeof int in
    assert_equal !@(from_voidp int (to_voidp p')) (succ i)
  done;

  (* Reverse traversal *)
  let pend = p +@ 7 in
  for i = 0 to 7 do
    assert_equal !@(pend -@ i) (8 - i)
  done


(*
  Test pointer comparisons.
*)
let test_pointer_comparison _ =
  let canonicalize p =
    (* Ensure that the 'pbyte_offset' component of the pointer is zero by
       writing the pointer to memory and then reading it back. *)
    let buf = allocate_n ~count:1 (ptr void) in
    buf <-@ (to_voidp p);
    !@buf
  in

  let (<) l r = ptr_compare l r < 0
  and (>) l r = ptr_compare l r > 0
  and (=) l r = ptr_compare l r = 0 in

  (* equal but not identical pointers compare equal *)
  let p = allocate int 10 in
  let p' = from_voidp int (to_voidp p) in
  assert_bool "equal but not identical poitners compare equal"
    (p = p');

  (* Canonicalization preserves ordering *)
  assert_bool "p < p+n"
    (p < (p +@ 10));

  assert_bool "canonicalize(p) < canonicalize(p+n)"
    (canonicalize p < canonicalize (p +@ 10));

  assert_bool "p > p-1"
    (p > (p -@ 1));

  assert_bool "canonicalize(p) > canonicalize(p-1)"
    (canonicalize p > canonicalize (p -@ 1));

  let s3 = structure "s3" in
  let i = field s3 "i" int in
  let j = field s3 "j" int in
  let k = field s3 "k" int in
  let () = seal s3 in

  let sp = addr (make s3) in
  let p1 = to_voidp (sp |-> i)
  and p2 = to_voidp (sp |-> j)
  and p3 = to_voidp (sp |-> k) in

  assert_bool "sp |-> i < sp |-> j"
    (p1 < p2);

  assert_bool "sp |-> i < canonicalize (sp |-> j)"
    (p1 < canonicalize p2);

  assert_bool "canonicalize (sp |-> i) < sp |-> j"
    (canonicalize p1 < p2);

  assert_bool "canonicalize (sp |-> i) < canonicalize (sp |-> j)"
    (canonicalize p1 < canonicalize p2);

  assert_bool "sp |-> i < sp |-> k"
    (p1 < p3);

  assert_bool "sp |-> i < canonicalize (sp |-> k)"
    (p1 < canonicalize p3);

  assert_bool "canonicalize (sp |-> i) < sp |-> k"
    (canonicalize p1 < p3);

  assert_bool "canonicalize (sp |-> i) < canonicalize (sp |-> k)"
    (canonicalize p1 < canonicalize p3);

  assert_bool "sp |-> j < sp |-> k"
    (p2 < p3);

  assert_bool "sp |-> j < canonicalize (sp |-> k)"
    (p2 < canonicalize p3);

  assert_bool "canonicalize (sp |-> j) < sp |-> k"
    (canonicalize p2 < p3);

  assert_bool "canonicalize (sp |-> j) < canonicalize (sp |-> k)"
    (canonicalize p2 < canonicalize p3);

  (* Canonicalization preserves equality *)
  assert_bool "canonicalization preserves equality"
    (to_voidp p = canonicalize p)


(*
  Test pointer differences.
*)
let test_pointer_differences _ =
  let canonicalize p =
    (* Ensure that the 'pbyte_offset' component of the pointer is zero by
       writing the pointer to memory and then reading it back. *)
    let buf = allocate_n ~count:1 (ptr void) in
    buf <-@ (to_voidp p);
    !@buf
  in

  let s = structure "s" in
  let (-:) ty label = field s label ty in
  let i = int           -: "i" in
  let j = array 17 char -: "j" in
  let k = double        -: "k" in
  let l = char          -: "l" in
  let () = seal s in

  let v = make s in
  let p = addr v in

  let to_charp p = from_voidp char (to_voidp p) in
  let cp = to_charp p in

  assert_equal (offsetof i) (ptr_diff cp (to_charp (p |-> i)));
  assert_equal (offsetof j) (ptr_diff cp (to_charp (p |-> j)));
  assert_equal (offsetof k) (ptr_diff cp (to_charp (p |-> k)));
  assert_equal (offsetof l) (ptr_diff cp (to_charp (p |-> l)));

  assert_equal (-offsetof i) (ptr_diff (to_charp (p |-> i)) cp);
  assert_equal (-offsetof j) (ptr_diff (to_charp (p |-> j)) cp);
  assert_equal (-offsetof k) (ptr_diff (to_charp (p |-> k)) cp);
  assert_equal (-offsetof l) (ptr_diff (to_charp (p |-> l)) cp);

  assert_equal (offsetof i) (ptr_diff cp (to_charp (canonicalize (p |-> i))));
  assert_equal (offsetof j) (ptr_diff cp (to_charp (canonicalize (p |-> j))));
  assert_equal (offsetof k) (ptr_diff cp (to_charp (canonicalize (p |-> k))));
  assert_equal (offsetof l) (ptr_diff cp (to_charp (canonicalize (p |-> l))));

  assert_equal (-offsetof i) (ptr_diff (to_charp (canonicalize (p |-> i))) cp);
  assert_equal (-offsetof j) (ptr_diff (to_charp (canonicalize (p |-> j))) cp);
  assert_equal (-offsetof k) (ptr_diff (to_charp (canonicalize (p |-> k))) cp);
  assert_equal (-offsetof l) (ptr_diff (to_charp (canonicalize (p |-> l))) cp)

(*
  Test raw pointers.
*)
let test_raw_pointers _ =
  (* Check that conversions to the raw form commute with arithmetic. *)
  let p : float ptr = allocate double 1.0 in
  let p' = p +@ 3 in
  let praw = raw_address_of_ptr (to_voidp p) in
  let praw' = raw_address_of_ptr (to_voidp p') in
  assert_equal praw' Nativeint.(add praw (of_int (3 * sizeof double)))


module Foreign_tests = Common_tests(Tests_common.Foreign_binder)

let suite = "Pointer tests" >:::
  ["passing pointers (foreign)"
    >:: Foreign_tests.test_passing_pointers;

   "passing pointers to pointers (foreign)"
    >:: Foreign_tests.test_passing_pointers_to_pointers;

   "callback receiving pointers (foreign)"
    >:: Foreign_tests.test_callback_receiving_pointers;

   "callback returning pointers (foreign)"
    >:: Foreign_tests.test_callback_returning_pointers;

   "pointer assignment with primitives"
    >:: test_pointer_assignment_with_primitives;

   "passing pointer to function pointer (foreign)"
    >:: Foreign_tests.test_passing_pointer_to_function_pointer;

   "callback returning pointer to function pointer (foreign)"
    >:: Foreign_tests.test_callback_returning_pointer_to_function_pointer;

   "incomplete types"
    >:: test_dereferencing_pointers_to_incomplete_types;

   "global value"
    >:: test_reading_and_writing_global_value;

   "passing pointers through functions (foreign)"
    >:: Foreign_tests.test_passing_pointer_through;

   "returned globals (foreign)"
    >:: Foreign_tests.test_reading_returned_global;

   "reading strings"
    >:: test_reading_strings;

   "arithmetic"
    >:: test_pointer_arithmetic;

   "comparisons"
    >:: test_pointer_comparison;

   "differences"
    >:: test_pointer_differences;

   "raw"
    >:: test_raw_pointers;
  ]



let _ =
  run_test_tt_main suite
