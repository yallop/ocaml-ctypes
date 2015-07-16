(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open OUnit2
open Ctypes
open Foreign


let testlib = Dl.(dlopen ~filename:"clib/libtest_functions.so" ~flags:[RTLD_NOW])


(*
  Test root lifetime.
*)
let test_root_lifetime _ =
  (* Check that values not registered as roots are collected. *)
  let alive = ref true in
  let v = [| 1; 2; 3 |] in
  Gc.finalise (fun _ -> alive := false) v;
  Gc.compact ();
  assert_equal false !alive
    ~msg:"values not registered as roots are collected";

  (* Check that values registered as roots are not collected. *)
  let alive = ref true in
  let v = [| 1; 2; 3 |] in
  Gc.finalise (fun _ -> alive := false) v;
  let _r = Root.create v in
  Gc.compact ();
  assert_equal true !alive
    ~msg:"registered roots are not collected";

  (* Check that values unregistered as roots are collected. *)
  let alive = ref true in
  let v = [| 1; 2; 3 |] in
  Gc.finalise (fun _ -> alive := false) v;
  let r = Root.create v in
  Root.release r;
  Gc.compact ();
  assert_equal false !alive
    ~msg:"released roots are collected";

  (* Check that values assigned to roots are not collected. *)
  let alive = ref true in
  let v = [| 1; 2; 3 |] in
  Gc.finalise (fun _ -> alive := false) v;
  let r = Root.create () in
  Root.set r v;
  Gc.compact ();
  assert_equal true !alive
    ~msg:"values assigned to roots are not collected";

  (* Check that values registered as roots and then overwritten are collected. *)
  let alive = ref true in
  let v = [| 1; 2; 3 |] in
  Gc.finalise (fun _ -> alive := false) v;
  let r = Root.create v in
  Root.set r ();
  Gc.compact ();
  assert_equal false !alive
    ~msg:"overwritten roots are collected";

  ()


let suite = "Root tests" >:::
  ["root lifetime"
    >:: test_root_lifetime;
  ]


let _ =
  run_test_tt_main suite
