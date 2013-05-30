open OUnit
open Ctypes


let testlib = Dl.(dlopen ~filename:"clib/test_functions.so" ~flags:[RTLD_NOW])


(*
  Retrieve a struct exposed as a global value. 
*)
let test_retrieving_string () =
  let open Struct in
  let open Ptr in
  let s = structure "global_struct" in
  let len = s *:* size_t in
  let str = s *:* array 1 char in
  let () = seals s in
  let global_struct = foreign_value "global_struct" s ~from:testlib in
  let p = Array.start (getf !global_struct str) in
  let stringp = from_voidp string (to_voidp (Ptr.make (ptr char) p)) in
  begin
    let expected = "global string" in
    assert_equal expected !stringp;
    assert_equal
      (Unsigned.Size_t.of_int (String.length expected))
      (getf !global_struct len)
  end


let suite = "Foreign value tests" >:::
  ["retrieving string" >:: test_retrieving_string;
  ]


let _ =
  run_test_tt_main suite
