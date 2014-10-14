(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Foreign function bindings for the threads tests. *)

open Ctypes
open Foreign

let () =
  (* temporary workaround due to flexlink limitations *)
  if Sys.os_type = "Win32" then
    Dl.(dlopen ~filename:"clib/libtest_functions.so" ~flags:[RTLD_NOW]) |> ignore

let initialize_waiters = foreign "initialize_waiters"
  (void @-> returning void)

let post1_wait2 = foreign "post1_wait2"
  ~release_runtime_lock:true
  (void @-> returning void)

let post2_wait1 = foreign "post2_wait1"
  ~release_runtime_lock:true
  (void @-> returning void)

let callback_with_pointers = foreign "passing_pointers_to_callback"
  ~release_runtime_lock:true
  (funptr ~runtime_lock:true
     (ptr int @-> ptr int @-> returning int) @-> returning int)
