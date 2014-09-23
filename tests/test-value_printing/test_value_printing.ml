(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open OUnit2
open Ctypes


let strip_whitespace = Str.(global_replace (regexp "[\n ]+") "")

let equal_ignoring_whitespace l r =
  strip_whitespace l = strip_whitespace r


module Common_tests(S : Cstubs.FOREIGN with type 'a fn = 'a) =
struct
  module M = Functions.Stubs(S)
  open M

  (*
    Test the printing of atomic values: arithmetic types and values of abstract
    types.
  *)
  let test_atomic_printing _ =
    (* char *)
    let _CHAR_MIN = retrieve_CHAR_MIN () in
    let _CHAR_MAX = retrieve_CHAR_MAX () in

    assert_equal (string_of char _CHAR_MIN) (Printf.sprintf "'%c'" _CHAR_MIN);
    assert_equal (string_of char 'a') "'a'";
    assert_equal (string_of char 'A') "'A'";
    assert_equal (string_of char '3') "'3'";
    assert_equal (string_of char '\n') "'\n'";
    assert_equal (string_of char ' ') "' '";
    assert_equal (string_of char _CHAR_MAX) (Printf.sprintf "'%c'" _CHAR_MAX);

    (* signed char *)
    let _SCHAR_MIN = retrieve_SCHAR_MIN () in
    let _SCHAR_MAX = retrieve_SCHAR_MAX () in

    assert_equal (string_of schar _SCHAR_MIN) (string_of_int _SCHAR_MIN);
    assert_equal (string_of schar 0) (string_of_int 0);
    assert_equal (string_of schar (-5)) (string_of_int (-5));
    assert_equal (string_of schar 5) (string_of_int 5);
    assert_equal (string_of schar _SCHAR_MAX) (string_of_int _SCHAR_MAX);

    (* short *)
    let _SHRT_MIN = retrieve_SHRT_MIN () in
    let _SHRT_MAX = retrieve_SHRT_MAX () in

    assert_equal (string_of short _SHRT_MIN) (string_of_int _SHRT_MIN);
    assert_equal (string_of short 0) (string_of_int 0);
    assert_equal (string_of short (-5)) (string_of_int (-5));
    assert_equal (string_of short 14) (string_of_int 14);
    assert_equal (string_of short _SHRT_MAX) (string_of_int _SHRT_MAX);

    (* int *)
    let _INT_MIN = retrieve_INT_MIN () in
    let _INT_MAX = retrieve_INT_MAX () in

    assert_equal (string_of int _INT_MIN) (string_of_int _INT_MIN);
    assert_equal (string_of int 0) (string_of_int 0);
    assert_equal (string_of int (-5)) (string_of_int (-5));
    assert_equal (string_of int 14) (string_of_int 14);
    assert_equal (string_of int _INT_MAX) (string_of_int _INT_MAX);

    (* int8_t *)
    let _INT8_MIN = retrieve_INT8_MIN () in
    let _INT8_MAX = retrieve_INT8_MAX () in

    assert_equal (string_of int8_t _INT8_MIN) (string_of_int _INT8_MIN);
    assert_equal (string_of int8_t 0) (string_of_int 0);
    assert_equal (string_of int8_t (-5)) (string_of_int (-5));
    assert_equal (string_of int8_t 14) (string_of_int 14);
    assert_equal (string_of int8_t _INT8_MAX) (string_of_int _INT8_MAX);

    (* int16_t *)
    let _INT16_MIN = retrieve_INT16_MIN () in
    let _INT16_MAX = retrieve_INT16_MAX () in

    assert_equal (string_of int16_t _INT16_MIN) (string_of_int _INT16_MIN);
    assert_equal (string_of int16_t 0) (string_of_int 0);
    assert_equal (string_of int16_t (-5)) (string_of_int (-5));
    assert_equal (string_of int16_t 14) (string_of_int 14);
    assert_equal (string_of int16_t _INT16_MAX) (string_of_int _INT16_MAX);

    (* int32_t *)
    let _INT32_MIN = retrieve_INT32_MIN () in
    let _INT32_MAX = retrieve_INT32_MAX () in

    assert_equal (string_of int32_t _INT32_MIN) (Int32.to_string _INT32_MIN);
    assert_equal (string_of int32_t 0l) (Int32.to_string 0l);
    assert_equal (string_of int32_t (-5l)) (Int32.to_string (-5l));
    assert_equal (string_of int32_t 14l) (Int32.to_string 14l);
    assert_equal (string_of int32_t _INT32_MAX) (Int32.to_string _INT32_MAX);

    (* int64_t *)
    let _INT64_MIN = retrieve_INT64_MIN () in
    let _INT64_MAX = retrieve_INT64_MAX () in

    assert_equal (string_of int64_t _INT64_MIN) (Int64.to_string _INT64_MIN);
    assert_equal (string_of int64_t 0L) (Int64.to_string 0L);
    assert_equal (string_of int64_t (-5L)) (Int64.to_string (-5L));
    assert_equal (string_of int64_t 14L) (Int64.to_string 14L);
    assert_equal (string_of int64_t _INT64_MAX) (Int64.to_string _INT64_MAX);

    (* float *)
    let _FLT_MIN = retrieve_FLT_MIN () in
    let _FLT_MAX = retrieve_FLT_MAX () in
    assert (_FLT_MAX > 0.0);

    assert_equal (string_of float _FLT_MIN) (string_of_float _FLT_MIN);
    assert_equal (valid_float_lexem (string_of float 0.0)) (string_of_float 0.0);
    assert_equal (string_of float nan) (string_of_float nan);
    assert_equal (string_of float infinity) (string_of_float infinity);
    assert_equal (string_of float _FLT_MAX) (string_of_float _FLT_MAX);

    (* double *)
    let _DBL_MIN = retrieve_DBL_MIN () in
    let _DBL_MAX = retrieve_DBL_MAX () in

    assert_equal (string_of double _DBL_MIN) (string_of_float _DBL_MIN);
    assert_equal (valid_float_lexem (string_of double 0.0)) (string_of_float 0.0);
    assert_equal (string_of double (-1.03)) (string_of_float (-1.03));
    assert_equal (string_of double (34.22)) (string_of_float (34.22));
    assert_equal (string_of double (1.39e16)) (string_of_float (1.39e16));
    assert_equal (string_of double nan) (string_of_float nan);
    assert_equal (string_of double infinity) (string_of_float infinity);
    assert_equal (string_of double _DBL_MAX) (string_of_float _DBL_MAX);

    ()
end


(*
  Test the printing of pointers.
*)
let test_pointer_printing _ =
  (* There's not much we can test here, since pointer formatting is
     implementation-dependent.  We can at least run the pointer-formatting
     code, and test that pointers of different types are printed
     equivalently. *)
  let arr = CArray.make int 10 in
  let p = CArray.start arr in

  assert_equal
    (string_of (ptr (reference_type p)) p)
    (string_of (ptr void) (to_voidp p))
    

(*
  Test the printing of structs.
*)
let test_struct_printing _ =
  let s = structure "s" in
  let (-:) ty label = field s label ty in
  let a = array 3 int -: "arr" in
  let d = double      -: "dbl" in
  let c = char        -: "chr" in
  let () = seal s in

  let t = structure "t" in
  let (-:) ty label = field t label ty in
  let ts = s   -: "ts" in
  let ti = int -: "ti" in
  let () = seal t in

  let vt = make t in
  let vs = make s in

  begin
    setf vs a (CArray.of_list int [4; 5; 6]);
    setf vs d nan;
    setf vs c 'a';

    setf vt ts vs;
    setf vt ti 14;

    assert_bool "struct printing"
      (equal_ignoring_whitespace
         "{ts = { arr = {4, 5, 6}, dbl = nan, chr = 'a' }, ti = 14}"
         (string_of t vt))
  end


(*
  Test the printing of unions.
*)
let test_union_printing _ =
  let s = structure "s" in
  let (-:) ty label = field s label ty in
  let i = int32_t -: "i" in
  let j = int32_t -: "j" in
  let () = seal s in
  let u = union "u" in
  let (-:) ty label = field u label ty in
  let us = s               -: "us" in
  let ua = array 4 int32_t -: "ua" in
  let () = seal u in
  let v = make u in
  ignore (i, j, us);
  setf v ua (CArray.make ~initial:0l int32_t 4);
  assert_bool "union printing"
    (equal_ignoring_whitespace "{ us = {i = 0, j = 0} | ua = {0, 0, 0, 0}}" (string_of u v))


(*
  Test the printing of array types.
*)
let test_array_printing _ =
  let arr = CArray.of_list int [-1; 0; 1] in
  let arrarr = CArray.of_list (array 3 int) [arr; arr] in
  assert_bool "array printing"
    (equal_ignoring_whitespace "{{-1, 0, 1}, {-1, 0, 1}}"
       (string_of (array 2 (array 3 int)) arrarr))


module Foreign_tests = Common_tests(Tests_common.Foreign_binder)
module Stub_tests = Common_tests(Generated_bindings)


let suite = "Value printing tests" >:::
  ["printing atomic values (foreign)"
    >:: Foreign_tests.test_atomic_printing;

   "printing atomic values (stubs)"
    >:: Stub_tests.test_atomic_printing;

   "printing pointers"
    >:: test_pointer_printing;

   "printing structs"
    >:: test_struct_printing;

   "printing unions"
    >:: test_union_printing;

   "printing arrays"
    >:: test_array_printing;
  ]


let _ =
  run_test_tt_main suite
