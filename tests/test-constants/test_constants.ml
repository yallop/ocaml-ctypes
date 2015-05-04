(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open OUnit2
open Ctypes


let testlib = Dl.(dlopen ~filename:"clib/libtest_functions.so" ~flags:[RTLD_NOW])


module Tests(T: Cstubs.Types.TYPE with type 'a const = 'a) =
struct
  module Constants = Types.Struct_stubs(T)

  let constant name typ =
    Foreign.foreign ~from:testlib ("retrieve_"^ name) (void @-> returning typ) ()

  let test_retrieve_constants _ =
    begin
      assert_equal Constants._LONG_MIN (constant "LONG_MIN" long);
      assert_equal Constants._SCHAR_MIN (constant "SCHAR_MIN" Ctypes.schar);
      assert_equal Constants._SCHAR_MAX (constant "SCHAR_MAX" Ctypes.schar);
      assert_equal Constants._UCHAR_MAX (constant "UCHAR_MAX" Ctypes.uchar);
      assert_equal Constants._CHAR_MIN (constant "CHAR_MIN" Ctypes.char);
      assert_equal Constants._CHAR_MAX (constant "CHAR_MAX" Ctypes.char);
      assert_equal Constants._SHRT_MIN (constant "SHRT_MIN" Ctypes.short);
      assert_equal Constants._SHRT_MAX (constant "SHRT_MAX" Ctypes.short);
      assert_equal Constants._USHRT_MAX (constant "USHRT_MAX" Ctypes.ushort);
      assert_equal Constants._UINT_MAX (constant "UINT_MAX" Ctypes.uint);
      assert_equal Constants._LONG_MAX (constant "LONG_MAX" Ctypes.long);
      assert_equal Constants._LONG_MIN (constant "LONG_MIN" Ctypes.long);
      assert_equal Constants._ULONG_MAX (constant "ULONG_MAX" Ctypes.ulong);
      assert_equal Constants._LLONG_MAX (constant "LLONG_MAX" Ctypes.llong);
      assert_equal Constants._LLONG_MIN (constant "LLONG_MIN" Ctypes.llong);
      assert_equal Constants._ULLONG_MAX (constant "ULLONG_MAX" Ctypes.ullong);
      assert_equal Constants._INT8_MIN (constant "INT8_MIN" Ctypes.int8_t);
      (* assert_equal Constants._INT16_MIN (constant "INT16_MIN" Ctypes.int16_t); *)
      assert_equal Constants._INT32_MIN (constant "INT32_MIN" Ctypes.int32_t);
      assert_equal Constants._INT64_MIN (constant "INT64_MIN" Ctypes.int64_t);
      assert_equal Constants._INT8_MAX (constant "INT8_MAX" Ctypes.int8_t);
      (* assert_equal Constants._INT16_MAX (constant "INT16_MAX" Ctypes.int16_t); *)
      assert_equal Constants._INT32_MAX (constant "INT32_MAX" Ctypes.int32_t);
      assert_equal Constants._INT64_MAX (constant "INT64_MAX" Ctypes.int64_t);
      assert_equal Constants._UINT8_MAX (constant "UINT8_MAX" Ctypes.uint8_t);
      assert_equal Constants._UINT16_MAX (constant "UINT16_MAX" Ctypes.uint16_t);
      assert_equal Constants._UINT32_MAX (constant "UINT32_MAX" Ctypes.uint32_t);
      assert_equal Constants._UINT64_MAX (constant "UINT64_MAX" Ctypes.uint64_t);
      assert_equal Constants._SIZE_MAX (constant "SIZE_MAX" Ctypes.size_t);
      assert_equal Constants._true true;
      assert_equal Constants._false false;
    end

  let test_retrieve_views _ =
    begin
      assert_equal
        Constants.neg_INT16_MAX
        (Int32.(neg (of_int (constant "INT16_MAX" Ctypes.int16_t))))
      ;

      assert_equal
        Constants.neg_INT16_MIN
        (Int32.(neg (of_int (constant "INT16_MIN" Ctypes.int16_t))))
      ;
    end

  let test_retrieve_enums _ =
    begin
      assert_equal
        [0; 1; 10; 11]
        Constants.([_A; _B; _C; _D])
    end
end

module Stubs_tests = Tests(Generated_struct_bindings)
module Simple_stubs_tests = Tests(Generated_simple_struct_bindings)

let suite = "Constant tests" >:::
  ["retrieving values of various integer types"
   >:: Stubs_tests.test_retrieve_constants;

   "retrieving values of view type"
   >:: Stubs_tests.test_retrieve_views;

   "retrieving enumeration constants"
   >:: Stubs_tests.test_retrieve_enums;

   "retrieving values of various integer types (simple stubs)"
   >:: Simple_stubs_tests.test_retrieve_constants;

   "retrieving values of view type (simple stubs)"
   >:: Simple_stubs_tests.test_retrieve_views;

   "retrieving enumeration constants (simple stubs)"
   >:: Simple_stubs_tests.test_retrieve_enums;
  ]


let _ =
  run_test_tt_main suite
