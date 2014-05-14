(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open OUnit
open Ctypes
open Foreign


let testlib = Dl.(dlopen ~filename:"clib/libtest_functions.so" ~flags:[RTLD_NOW])

module Common_tests(S : Cstubs.FOREIGN with type 'a fn = 'a) =
struct
  module M = Functions.Stubs(S)
  open M

  (*
    Test passing OCaml strings directly to C.
  *)
  let test_passing_strings () =
    let input = "abcdefghijklmnopqrstuvwxyz" in
    let len = String.length input in
    let buf = String.create len in
    let _ = memcpy_string_string
      (ocaml_string_start buf)
      (ocaml_string_start input)
      (Unsigned.Size_t.of_int len)
    in begin
      assert_equal buf input
    end;
    
    let arr = CArray.make char len in
    let () = String.iteri (CArray.set arr) input in
    let buf = String.create len in
    let _ = memcpy_string_ptr
      (ocaml_string_start buf)
      (coerce (ptr char) (ptr void) (CArray.start arr))
      (Unsigned.Size_t.of_int len)
    in begin
      assert_equal buf input
    end


    (*
      Test pointer arithmetic on OCaml values.
    *)
    let test_pointer_arithmetic () =
      let s = ocaml_string_start "abcdefghijklmnopqrstuvwxyz" in
      begin
        assert_equal s (s +@ 0);

        assert_equal (ptr_diff s (s +@ 10)) 10;

        assert_equal s ((s +@ 10) -@ 10);

        assert_equal
          (strdup (ocaml_string_start "klmnopqrstuvwxyz"))
          (strdup (s +@ 10))
      end
end


(*
  Test that OCaml values do not reside in addressable memory.
*)
let test_ocaml_types_rejected_as_pointer_reference_types () =
  assert_raises IncompleteType
    (fun () -> allocate ocaml_string (ocaml_string_start ""))


module Foreign_tests = Common_tests(Tests_common.Foreign_binder)
module Stub_tests = Common_tests(Generated_bindings)


let suite = "Tests passing OCaml values" >:::
  ["passing strings (foreign)"
    >:: Foreign_tests.test_passing_strings;

   "passing strings (stubs)"
    >:: Stub_tests.test_passing_strings;

   "pointer arithmetic on OCaml values (foreign)"
    >:: Foreign_tests.test_pointer_arithmetic;

   "pointer arithmetic on OCaml values (stubs)"
    >:: Stub_tests.test_pointer_arithmetic;

   "ocaml_string values aren't addressable"
    >:: test_ocaml_types_rejected_as_pointer_reference_types;
  ]



let _ =
  run_test_tt_main suite
