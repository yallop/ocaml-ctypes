open OUnit


(* Tests for the low-level module on which the public high-level
   interface is based.  
*)


(* Call the C function

        double fabs(double)
*)
let test_fabs () =
  Ffi_raw.(Types.(
    let callspec = allocate_callspec () in
    let arg_1_offset = Ffi_raw.add_argument callspec double in
    let () = prep_callspec callspec double in
    
    let dlfabs = Dl.dlsym "fabs" in
    
    let fabs x =
      call dlfabs callspec
        (write ~offset:arg_1_offset double x)
        (read ~offset:0 double)
    in

    assert_equal 2.0 (fabs (-2.0)) ~printer:string_of_float;
    assert_equal 12.0 (fabs (12.0)) ~printer:string_of_float;
    assert_equal 0.0 (fabs 0.0) ~printer:string_of_float;
  ))


(* Call the C function

        double pow(double, double)
*)
let test_pow () =
  Ffi_raw.(Types.(
    let callspec = allocate_callspec () in
    let arg_1_offset = Ffi_raw.add_argument callspec double in
    let arg_2_offset = Ffi_raw.add_argument callspec double in
    let () = prep_callspec callspec double in
    
    let dlpow = Dl.dlsym "pow" in
    
    let pow x y =
      call dlpow callspec
        (fun buffer ->
          write arg_1_offset double x buffer;
          write arg_2_offset double y buffer)
        (read ~offset:0 double)
    in

    assert_equal 8.0 (pow 2.0 3.0);
    assert_equal 1.0 (pow 10.0 0.0);
  ))


let suite = "Raw interface tests" >:::
  ["test_abs" >:: test_fabs;
   "test_pow" >:: test_pow]


let _ =
  run_test_tt_main suite
