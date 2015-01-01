(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open OUnit2
open Ctypes


module Build_enum_stub_tests
    (S : Cstubs.Types.TYPE
          with type 'a typ = 'a Ctypes.typ
           and type ('a, 's) field = ('a, 's) Ctypes.field) =
struct
  module M = Types.Struct_stubs(S)
  open M

  module Build_call_tests
      (F : Cstubs.FOREIGN with type 'a fn = 'a) =
  struct
    module F = Functions.Stubs(F)
    open F
    open M

    let test_passing_returning_enums _ =
      let open Types in
      begin
        assert_equal Apple  (next_fruit Orange);
        assert_equal Banana (next_fruit Apple);
        assert_equal Pear   (next_fruit Banana);
        assert_equal Orange (next_fruit Pear);
      end

    let test_signed_enums _ =
      begin
        assert_equal (-1)  (classify_integer (-3));
        assert_equal 1     (classify_integer 4);
      end

    let test_default_enums _ =
      begin
        assert_equal 0 (out_of_range ())
      end

  end

end

module Enum_stubs_tests = Build_enum_stub_tests(Generated_struct_bindings)
module Combined_stub_tests = Enum_stubs_tests.Build_call_tests(Generated_bindings)


let suite = "Enum tests" >:::
  [
    "passing and returning enums"
    >:: Combined_stub_tests.test_passing_returning_enums;

    "enums with signed values"
    >:: Combined_stub_tests.test_signed_enums;

    "enums with default values"
    >:: Combined_stub_tests.test_default_enums;
  ]


let _ =
  run_test_tt_main suite
