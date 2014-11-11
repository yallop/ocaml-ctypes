(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open OUnit2
open Ctypes


module Common_tests(S : Tests_common.FOREIGN with type 'a fn = 'a) =
struct
  module M = Functions.Stubs(S)
  open M

  (*
    Call a function of type

       int (int)

    using a custom view that treats chars as ints.
  *)
  let test_passing_chars_as_ints _ =
    assert_equal ~msg:"toupper('x') = 'X'"
      'X' (toupper 'x');

    assert_equal ~msg:"toupper('3') = '3'"
      '3' (toupper '3');

    assert_equal ~msg:"toupper('X') = 'X'"
      'X' (toupper 'X')


  (*
    Use views to create a nullable function pointer.
  *)
  let test_nullable_function_pointer_view _ =
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
end

(*
  Use the nullable pointer view to view nulls as Nones.
*)
let test_nullable_pointer_view _ =
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


module Foreign_tests = Common_tests(Tests_common.Foreign_binder)


let suite = "View tests" >:::
  ["custom views (foreign)"
   >:: Foreign_tests.test_passing_chars_as_ints;

   "nullable function pointers (foreign)"
   >:: Foreign_tests.test_nullable_function_pointer_view;

   "nullable pointers"
    >:: test_nullable_pointer_view;
  ]


let _ =
  run_test_tt_main suite
