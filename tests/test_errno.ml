open OUnit
open Ctypes


(*
  Call fdopendir() with a bogus file descriptor and check that an exception is raised. 
*)
let test_errno_exception_raised () =
  let fdopendir = foreign "fdopendir" (int @-> syscall (ptr void)) in
  assert_raises (Unix.Unix_error(Unix.EBADF, "fdopendir", ""))
    (fun () -> fdopendir (-300))
    

(*
  Call chdir() with a valid directory path and check that zero is returned. 
*)
let test_int_return_errno_exception_raised () =
  let chdir = foreign "chdir" (string @-> syscall int) in
  assert_raises (Unix.Unix_error(Unix.ENOENT, "chdir", ""))
    (fun () -> chdir "/unlikely_to_exist")
    

(*
  Call chdir() with a valid directory path and check that zero is returned. 
*)
let test_errno_no_exception_raised () =
  let chdir = foreign "chdir" (string @-> syscall int) in
  assert_equal 0 (chdir (Sys.getcwd ()))

    

let suite = "errno tests" >:::
  ["Exception from fdopendir" >:: test_errno_exception_raised;
   "Exception from chdir" >:: test_int_return_errno_exception_raised;
   "No exception from chdir" >:: test_errno_no_exception_raised;
  ]


let _ =
  run_test_tt_main suite
