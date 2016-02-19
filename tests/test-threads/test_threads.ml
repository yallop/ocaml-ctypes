(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Ctypes
open OUnit2
open Foreign
open Functions


(*
  Ensure that passing ~release_runtime_lock releases the runtime lock.
*)
let test_release_runtime_lock _ =
  begin
    initialize_waiters ();
    let t1 = Thread.create post1_wait2 () in
    let t2 = Thread.create post2_wait1 () in
    Thread.join t1;
    Thread.join t2;
  end


(*
  Ensure that passing ~runtime_lock to funptr causes a callback to acquire
  the runtime lock.
*)
let test_acquire_runtime_lock _ =
  begin
    let f x y = let _ = Gc.major () in !@x + !@y in
    let t1 = Thread.create Gc.major () in
    assert (callback_with_pointers f = 7);
    Thread.join t1
  end


let suite = "Thread tests" >:::
  ["test_release_runtime_lock (foreign)"
   >:: test_release_runtime_lock;

   "test_acquire_runtime_lock (foreign)"
   >:: test_acquire_runtime_lock;
  ]


let _ =
  run_test_tt_main suite
