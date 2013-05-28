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
  let concat = foreign "concat_strings" ~from:testlib
    (ptr string @-> int @-> ptr char @-> returning void) in

  let l = ["the "; "quick "; "brown "; "fox "; "etc. "; "etc. "; ] in
  let arr = Array.of_list string l in

  let outlen = List.fold_left (fun a s -> String.length s + a) 1 l in
  let buf = Array.make char outlen in

  let () = Array.(concat (start arr) (length arr) (start buf)) in
  let buf_addr = Ptr.make (ptr char) (Array.start buf) in
  let s = Ptr.from_voidp string (Ptr.to_voidp buf_addr) in

  assert_equal ~msg:"Check output"
    "the quick brown fox etc. etc. " Ptr.(!s)


(*
  Call a function of type

     int (int)

  using a custom view that treats chars as ints.
*)
let test_passing_chars_as_ints () =
  let charish = view ~read:Char.chr ~write:Char.code int in
  let toupper = foreign "toupper" (charish @-> returning charish) in

  assert_equal ~msg:"toupper('x') = 'X'"
    'X' (toupper 'x');

  assert_equal ~msg:"toupper('3') = '3'"
    '3' (toupper '3');

  assert_equal ~msg:"toupper('X') = 'X'"
    'X' (toupper 'X')


let suite = "View tests" >:::
  ["passing array of strings" >:: test_passing_string_array;
   "custom views" >:: test_passing_chars_as_ints;
  ]


let _ =
  run_test_tt_main suite
