(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open OUnit2
open Ctypes


(*
  Test that primitives are passable.
*)
let test_primitives_are_passable _ =
  let _ = void @-> returning void 
  and _ = char @-> returning char
  and _ = schar @-> returning schar
  and _ = float @-> returning float
  and _ = double @-> returning double
  and _ = int   @-> returning int  
  and _ = nativeint @-> returning nativeint
  and _ = int8_t @-> returning int8_t
  and _ = short @-> returning short
  and _ = int16_t @-> returning int16_t
  and _ = int32_t @-> returning int32_t
  and _ = int64_t @-> returning int64_t
  in ()




(*
  Test that function pointers are passable
*)
let test_function_pointers_are_passable _ =
  (* Pointers to primitives are passable *)
  ignore (Foreign.funptr (int @-> returning int)
          @-> returning (Foreign.funptr (int @-> returning int)))

(*
  Test passability of incomplete types.  Trying to use an incomplete type
  in a function specification should give rise to an error.
*)
let test_incomplete_passability _ =
  let s = structure "incomplete"
  in begin
    assert_raises IncompleteType
      (fun () -> s @-> returning void);
    
    assert_raises IncompleteType
      (fun () -> void @-> returning s);
  end


let suite = "Passability tests" >:::
  ["primitives are passable"
    >:: test_primitives_are_passable;

   "function pointers are passable"
    >:: test_function_pointers_are_passable;

   "incomplete types are not passable"
    >:: test_incomplete_passability;
  ]


let _ =
  run_test_tt_main suite
