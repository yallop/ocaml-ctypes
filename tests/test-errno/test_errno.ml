(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open OUnit2
open Ctypes


(*
   Call fdopendir() with a bogus file descriptor and check that an exception
   is raised.
*)
let test_errno_exception_raised _ =
  let close = Foreign.foreign "close" ~check_errno:true
    (int @-> returning int) in
  assert_raises (Unix.Unix_error(Unix.EBADF, "close", ""))
    (fun () -> close (-300))
    

(*
  Call chdir() with a valid directory path and check that zero is returned. 
*)
let test_int_return_errno_exception_raised _ =
  let chdir = Foreign.foreign "chdir" ~check_errno:true
    (string @-> returning int) in
  assert_raises (Unix.Unix_error(Unix.ENOENT, "chdir", ""))
    (fun () -> chdir "/unlikely_to_exist")
    

(*
  Call chdir() with a valid directory path and check that zero is returned. 
*)
let test_errno_no_exception_raised _ =
  let chdir = Foreign.foreign "chdir" ~check_errno:true
    (string @-> returning int) in
  assert_equal 0 (chdir (Sys.getcwd ()))

    

let suite = "errno tests" >:::
  ["Exception from fdopendir"
    >:: test_errno_exception_raised;

   "Exception from chdir"
   >:: test_int_return_errno_exception_raised;

   "No exception from chdir"
   >:: test_errno_no_exception_raised;
  ]


let _ =
  run_test_tt_main suite
