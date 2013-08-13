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
  Retrieve a struct exposed as a global value. 
*)
let test_retrieving_struct () =
  let s = structure "global_struct" in
  let (-:) ty label = field s label ty in
  let len = size_t       -: "len" in
  let str = array 1 char -: "str" in
  let () = seal s in
  let global_struct = Foreign.foreign_value "global_struct" s ~from:testlib in
  let p = Array.start (getf !@global_struct str) in
  let stringp = from_voidp string (to_voidp (allocate (ptr char) p)) in
  begin
    let expected = "global string" in
    assert_equal expected !@stringp;
    assert_equal
      (Unsigned.Size_t.of_int (String.length expected))
      (getf !@global_struct len)
  end


let suite = "Foreign value tests" >:::
  ["retrieving global struct"
    >:: test_retrieving_struct;
  ]


let _ =
  run_test_tt_main suite
