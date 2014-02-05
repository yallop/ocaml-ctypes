(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open OUnit
open Memory_stubs
open Std_view_stubs

(* Tests for the low-level module on which the public high-level
   interface is based.  
*)


(* Call the C function

        double fabs(double)
*)
let test_fabs () =
  Ffi_stubs.(
    let double_ffitype = primitive_ffitype Primitives.Double in
    let callspec = allocate_callspec () in
    let arg_1_offset = add_argument callspec double_ffitype in
    let () = prep_callspec callspec Libffi_abi.(abi_code default_abi)
      double_ffitype in
    
    let dlfabs = Dl.dlsym "fabs" in
    
    let fabs x =
      call dlfabs callspec
        (write Primitives.Double ~offset:arg_1_offset x)
        (read Primitives.Double ~offset:0)
    in

    assert_equal 2.0 (fabs (-2.0)) ~printer:string_of_float;
    assert_equal 12.0 (fabs (12.0)) ~printer:string_of_float;
    assert_equal 0.0 (fabs 0.0) ~printer:string_of_float;
  )


(* Call the C function

        double pow(double, double)
*)
let test_pow () =
  Ffi_stubs.(
    let double_ffitype = primitive_ffitype Primitives.Double in
    let callspec = allocate_callspec () in
    let arg_1_offset = add_argument callspec double_ffitype in
    let arg_2_offset = add_argument callspec double_ffitype in
    let () = prep_callspec callspec Libffi_abi.(abi_code default_abi) 
      double_ffitype in
    
    let dlpow = Dl.dlsym "pow" in
    
    let pow x y =
      call dlpow callspec
        (fun buffer ->
          write Primitives.Double ~offset:arg_1_offset x buffer;
          write Primitives.Double ~offset:arg_2_offset y buffer)
        (read ~offset:0 Primitives.Double)
    in

    assert_equal 8.0 (pow 2.0 3.0);
    assert_equal 1.0 (pow 10.0 0.0);
  )


let suite = "Raw interface tests" >:::
  ["test_abs"
    >:: test_fabs;

   "test_pow"
   >:: test_pow
  ]


let _ =
  run_test_tt_main suite
