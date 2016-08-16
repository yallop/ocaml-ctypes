(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Ctypes
open OUnit2
open Foreign

let () =
  (* temporary workaround due to flexlink limitations *)
  if Sys.os_type = "Win32" then
    ignore (Dl.(dlopen ~filename:"clib/libtest_functions.so" ~flags:[RTLD_NOW]))


let callback_with_pointers = Foreign.foreign "passing_pointers_to_callback"
  ~release_runtime_lock:true
  (Foreign.funptr ~runtime_lock:true
     (ptr int @-> ptr int @-> returning int) @-> returning int)


module Common_tests(S : Cstubs.FOREIGN with type 'a result = 'a
                                        and type 'a return = 'a) =
struct
  module M = Functions.Stubs(S)

  (*
    Ensure that passing ~release_runtime_lock releases the runtime lock.
  *)
  let test_release_runtime_lock _ =
    begin
      M.initialize_waiters ();
      let t1 = Thread.create M.post1_wait2 () in
      let t2 = Thread.create M.post2_wait1 () in
      Thread.join t1;
      Thread.join t2;
    end
end

module Foreign_tests = Common_tests(struct 
    type 'a fn = 'a Ctypes.fn
    type 'a return = 'a
    let (@->) = Ctypes.(@->)
    let returning = Ctypes.returning
                      
    type 'a result = 'a
    let foreign name fn = Foreign.foreign name fn
    ~release_runtime_lock:true
    let foreign_value name fn = Foreign.foreign_value name fn
  end)

module Stub_tests = Common_tests(Generated_bindings)


(*
  Ensure that passing ~runtime_lock to funptr causes a callback to acquire
  the runtime lock.
*)
let test_acquire_runtime_lock _ =
  begin
    let f x y = let _ = Gc.full_major () in !@x + !@y in
    let t1 = Thread.create Gc.full_major () in
    assert (callback_with_pointers f = 7);
    Thread.join t1
  end


(* 
   Acquire the runtime lock in a callback while other threads execute OCaml
   code.
*)
let test_acquire_runtime_lock_parallel _ =
  begin
    let r = ref None in
    let g size n =
      for i = 0 to n do
        r := Some (CArray.make float size ~initial:0.0);
        Thread.yield ();
      done
    in
    let f x y = let _ = Gc.compact () in !@x + !@y in
    let threads = ref [] in
    for i  = 0 to 10 do
      threads := Thread.create (g 100) 10000 :: !threads;
    done;
    for i = 0 to 10 do
      assert (callback_with_pointers f = 7);
      Thread.yield ();
    done;
    List.iter Thread.join !threads;
  end


let suite = "Thread tests" >:::
  ["test_release_runtime_lock (foreign)"
   >:: Foreign_tests.test_release_runtime_lock;

   "test_release_runtime_lock (stubs)"
   >:: Stub_tests.test_release_runtime_lock;

   "test_acquire_runtime_lock (foreign)"
   >:: test_acquire_runtime_lock;

   "test_acquire_runtime_lock_parallel (foreign)"
   >:: test_acquire_runtime_lock_parallel;
  ]


let _ =
  run_test_tt_main suite
