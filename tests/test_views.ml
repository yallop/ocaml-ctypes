(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open OUnit
open Ctypes


let testlib = Dl.(dlopen ~filename:"clib/test_functions.so" ~flags:[RTLD_NOW])


(*
  Call a function of type

     void (char **sv, int sc, char *buffer)

  using strings for input parameters and a char array for an output
  parameter.  Examine the output buffer using a cast to a string view.
*)
let test_passing_string_array () =
  let concat = Foreign.foreign "concat_strings" ~from:testlib
    (ptr string @-> int @-> ptr char @-> returning void) in

  let l = ["the "; "quick "; "brown "; "fox "; "etc. "; "etc. "; ] in
  let arr = Array.of_list string l in

  let outlen = List.fold_left (fun a s -> String.length s + a) 1 l in
  let buf = Array.make char outlen in

  let () = Array.(concat (start arr) (length arr) (start buf)) in
  let buf_addr = allocate (ptr char) (Array.start buf) in
  let s = from_voidp string (to_voidp buf_addr) in

  assert_equal ~msg:"Check output"
    "the quick brown fox etc. etc. " !@s


(*
  Call a function of type

     int (int)

  using a custom view that treats chars as ints.
*)
let test_passing_chars_as_ints () =
  let charish = view ~read:Char.chr ~write:Char.code int in
  let toupper = Foreign.foreign "toupper" (charish @-> returning charish) in

  assert_equal ~msg:"toupper('x') = 'X'"
    'X' (toupper 'x');

  assert_equal ~msg:"toupper('3') = '3'"
    '3' (toupper '3');

  assert_equal ~msg:"toupper('X') = 'X'"
    'X' (toupper 'X')


(*
  Use views to create a nullable function pointer.
*)
let test_nullable_function_pointer_view () =
  let nullable_intptr = funptr_opt (int @-> int @-> returning int) in
  let returning_funptr =
    Foreign.foreign "returning_funptr" ~from:testlib
      (int @-> returning nullable_intptr)
  and accepting_possibly_null_funptr =
    Foreign.foreign "accepting_possibly_null_funptr" ~from:testlib
      (nullable_intptr @-> int @-> int @-> returning int)
  in
  begin
    let fromSome = function None -> assert false | Some x -> x in

    let add = fromSome (returning_funptr 0)
    and times = fromSome (returning_funptr 1) in

    assert_equal ~msg:"reading non-null function pointer return value"
      9 (add 5 4);

    assert_equal ~msg:"reading non-null function pointer return value"
      20 (times 5 4);

    assert_equal ~msg:"reading null function pointer return value"
      None (returning_funptr 2);

    assert_equal ~msg:"passing null function pointer"
      (-1) (accepting_possibly_null_funptr None 2 3);

    assert_equal ~msg:"passing non-null function pointer"
      5 (accepting_possibly_null_funptr (Some Pervasives.(+)) 2 3);

    assert_equal ~msg:"passing non-null function pointer obtained from C"
      6 (accepting_possibly_null_funptr (returning_funptr 1) 2 3);
  end


(*
  Use the nullable pointer view to view nulls as Nones.
*)
let test_nullable_pointer_view () =
  let p = allocate int 10 in
  let pp = allocate (ptr int) p in
  let npp = from_voidp (ptr_opt int) (to_voidp pp) in
  begin
    assert_equal 10 !@ !@pp;

    begin match !@npp with
      | Some x -> assert_equal 10 !@x
      | None -> assert false
    end;
    
    pp <-@ from_voidp int null;

    assert_equal null (to_voidp !@pp);
    assert_equal None !@npp;
  end


let suite = "View tests" >:::
  ["passing array of strings"
   >:: test_passing_string_array;

   "custom views"
   >:: test_passing_chars_as_ints;

   "nullable function pointers"
   >:: test_nullable_function_pointer_view;

   "nullable pointers"
   >:: test_nullable_pointer_view;
  ]


let _ =
  run_test_tt_main suite
