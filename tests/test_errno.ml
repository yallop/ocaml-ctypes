open OUnit
open Ctypes


(*
  Call fdopendir() with a bogus file descriptor and check that an exception is raised. 
*)
let test_errno_exception_raised () =
  let fdopendir = foreign "fdopendir" Type.(int @-> syscall (ptr void)) in
  (* assert_raises (Unix.Unix_error (0, "", "")) *)
  assert_raises (Unix.Unix_error(Unix.EBADF, "fdopendir", ""))
    (fun () -> fdopendir (-300))
    

let suite = "errno tests" >:::
  ["Exception from fdopendir" >:: test_errno_exception_raised;
  ]


let _ =
  run_test_tt_main suite
