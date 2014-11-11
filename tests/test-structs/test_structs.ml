(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open OUnit2
open Ctypes


let testlib = Dl.(dlopen ~filename:"clib/libtest_functions.so" ~flags:[RTLD_NOW])



(*
  Check that attempts to use incomplete types for struct members are rejected.
*)
let test_incomplete_struct_members _ =
  let s = structure "s" in begin

    assert_raises IncompleteType
      (fun () -> field s "_" void);

    assert_raises IncompleteType
      (fun () -> field s "_" (structure "incomplete"));
  end


(*
  Test reading and writing pointers to struct members.
*)
let test_pointers_to_struct_members _ =
  let module M = struct
    type s

    let styp : s structure typ = structure "s"
    let (-:) ty label = field styp label ty
    let i = int     -: "i"
    let j = int     -: "j"
    let k = ptr int -: "k"
    let () = seal styp

    let s = make styp

    let () = begin
      let sp = addr s in
      sp |-> i <-@ 10;
      sp |-> j <-@ 20;
      (sp |-> k) <-@ (sp |-> i);
      assert_equal ~msg:"sp->i = 10" ~printer:string_of_int
        10 (!@(sp |-> i));
      assert_equal ~msg:"sp->j = 20" ~printer:string_of_int
        20 (!@(sp |-> j));
      assert_equal ~msg:"*sp->k = 10" ~printer:string_of_int
        10 (!@(!@(sp |-> k)));
      (sp |-> k) <-@ (sp |-> j);
      assert_equal ~msg:"*sp->k = 20" ~printer:string_of_int
        20 (!@(!@(sp |-> k)));
      sp |-> i <-@ 15;
      sp |-> j <-@ 25;
      assert_equal ~msg:"*sp->k = 25" ~printer:string_of_int
        25 (!@(!@(sp |-> k)));
      (sp |-> k) <-@ (sp |-> i);
      assert_equal ~msg:"*sp->k = 15" ~printer:string_of_int
        15 (!@(!@(sp |-> k)));
    end
  end in ()


(*
  Test that attempting to update a sealed struct is treated as an error.
*)
let test_updating_sealed_struct _ =
  let styp = structure "sealed" in
  let _ = field styp "_" int in
  let () = seal styp in

  assert_raises (ModifyingSealedType "sealed")
    (fun () -> field styp "_" char)


(*
  Test that attempting to seal an empty struct is treated as an error.
*)
let test_sealing_empty_struct _ =
  let empty = structure "empty" in

  assert_raises (Unsupported "struct with no fields")
    (fun () -> seal empty)


(* 
   Check that references to fields aren't garbage collected while they're
   still needed.
*)
let test_field_references_not_invalidated _ =
  let module M = struct
    type s1 and s2

    (*
      struct s1 {
        struct s2 {
          int i;
        } s2;
      };
    *)
    let s1 : s1 structure typ = structure "s1"
    let () = (fun () ->
      let s2 : s2 structure typ = structure "s2" in
      let _ = field s2 "i" int in
      let () = seal s2 in
      let _ = field s1 "_" s2 in
      ()
    ) ()
    let () = begin
      Gc.major ();
      seal s1;
      assert_equal ~printer:string_of_int
        (sizeof int) (sizeof s1)
    end
  end in ()


let suite = "Struct tests" >:::
  ["incomplete struct members rejected"
   >:: test_incomplete_struct_members;

   "pointers to struct members"
   >:: test_pointers_to_struct_members;

   "updating sealed struct"
   >:: test_updating_sealed_struct;

   "sealing empty struct"
   >:: test_sealing_empty_struct;

   "field references not invalidated"
   >:: test_field_references_not_invalidated;
  ]


let _ =
  run_test_tt_main suite
