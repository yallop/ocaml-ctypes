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
  Test primitive operations on complex numbers.

  Arguments and return values are currently mediated through pointers,
  since libffi doesn't support passing complex numbers.
*)
let test_complex_primitive_operations () =
  let open Foreign in
  let wrap typ name =
    let f = foreign name ~from:testlib
      (ptr typ @-> ptr typ @-> ptr typ @-> returning void) in
    fun l r ->
      let rv = allocate_n ~count:1 typ in
      f (allocate typ l) (allocate typ r) rv;
      !@rv
  in

  let addz64 = wrap complex64 "add_complexd"
  and mulz64 = wrap complex64 "mul_complexd"
  and addz32 = wrap complex32 "add_complexf"
  and mulz32 = wrap complex32 "mul_complexf"
  in

  begin
    let open Complex in

    let eps64 = 1e-12 in
    let complex64_eq { re = lre; im = lim } { re = rre; im = rim } =
      abs_float (lre -. rre) < eps64 && abs_float (lim -. rim) < eps64 in

    let eps32 = 1e-6 in
    let complex32_eq { re = lre; im = lim } { re = rre; im = rim } =
      abs_float (lre -. rre) < eps32 && abs_float (lim -. rim) < eps32 in

    let l = { re = 3.5; im = -1.0 } and r = { re = 2.0; im = 2.7 } in

    assert_equal ~cmp:complex64_eq (Complex.add l r) (addz64 l r);
    assert_equal ~cmp:complex64_eq (Complex.mul l r) (mulz64 l r);

    assert_equal ~cmp:complex32_eq (Complex.add l r) (addz32 l r);
    assert_equal ~cmp:complex32_eq (Complex.mul l r) (mulz32 l r);
  end


let suite = "Complex number tests" >:::
  ["basic operations on complex numbers"
    >:: test_complex_primitive_operations;
  ]


let _ =
  run_test_tt_main suite
