(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open OUnit2
open Ctypes
open Foreign


module Common_tests(S : Cstubs.FOREIGN with type 'a fn = 'a) =
struct
  module M = Functions.Stubs(S)
  open M

  (*
    Call the functions

       int isisalnum(int)
       int isisalpha(int)
       int isiscntrl(int)
       int isisdigit(int)
       int isisgraph(int)
       int isislower(int)
       int isisprint(int)
       int isispunct(int)
       int isisspace(int)
       int isisupper(int)
       int isisxdigit(int)
  *)
  let test_isX_functions _ =
    begin
      assert_bool "" (isalnum 'a');
      assert_bool "" (not (isalnum ' '));

      assert_bool "" (isalpha 'x');
      assert_bool "" (not (isalpha ';'));

      assert_bool "" (iscntrl '\r');
      assert_bool "" (not (iscntrl 'a'));

      assert_bool "" (isdigit '2');
      assert_bool "" (not (isdigit 'a'));

      assert_bool "" (isgraph '?');
      assert_bool "" (not (isgraph ' '));

      assert_bool "" (islower 's');
      assert_bool "" (not (islower 'S'));

      assert_bool "" (isprint ' ');
      assert_bool "" (not (isprint '\b'));

      assert_bool "" (ispunct '.');
      assert_bool "" (not (ispunct 'a'));

      assert_bool "" (isspace '\t');
      assert_bool "" (not (isspace '~'));

      assert_bool "" (isupper 'X');
      assert_bool "" (not (isupper 'x'));

      assert_bool "" (isxdigit 'f');
      assert_bool "" (not (isxdigit 'g'));
    end


  (*
    Call the functions

      char *strchr(const char *str, int c);
      int strcmp(const char *str1, const char *str2);
  *)
  let test_string_functions _ =
    assert_equal "efg" (strchr "abcdefg" (Char.code 'e'))
      ~printer:(fun x -> x);

    (* non-word-aligned pointers do not trigger exceptions *)
    assert_equal "defg" (strchr "abcdefg" (Char.code 'd'));

    assert_bool "strcmp('abc', 'def') < 0"
      (strcmp "abc" "def" < 0);

    assert_bool "strcmp('def', 'abc') > 0"
      (strcmp "def" "abc" > 0);

    assert_bool "strcmp('abc', 'abc') == 0"
      (strcmp "abc" "abc" = 0)
end

(*
  Call the functions

     div_t div(int numerator, int denominator)

  where div_t is defined as follows:

    typedef struct
      {
        int quot;			/* Quotient.  */
        int rem;			/* Remainder.  */
      } div_t;
*)
let test_div _ =
  let module M = struct
    type div_t
    let div_t : div_t structure typ = structure "div_t"
    let (-:) ty label = field div_t label ty
    let quot = int -: "quot"
    let rem  = int -: "rem"
    let () = seal div_t

    let div = foreign "div" (int @-> int @-> returning div_t)

    let test ~num ~dem ~quotient ~remainder =
      let v = div num dem in
      let () = assert_equal quotient (getf v quot) in
      let () = assert_equal remainder (getf v rem) in
      ()

    let () = test ~num:10 ~dem:2 ~quotient:5 ~remainder:0

    let () = test ~num:11 ~dem:2 ~quotient:5 ~remainder:1
  end in ()


module Foreign_tests = Common_tests(Tests_common.Foreign_binder)
module Stub_tests = Common_tests(Generated_bindings)


let suite = "C standard library tests" >:::
  ["test isX functions (foreign)"
    >:: Foreign_tests.test_isX_functions;

   "test isX functions (stubs)"
    >:: Stub_tests.test_isX_functions;

   "test string function (foreign)"
    >:: Foreign_tests.test_string_functions;

   "test string function (stubs)"
    >:: Stub_tests.test_string_functions;

   "test div function"
    >:: test_div;
  ]


let _ =
  run_test_tt_main suite
