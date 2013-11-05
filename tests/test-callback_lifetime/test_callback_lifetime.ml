(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open OUnit
open Ctypes
open Foreign


let testlib = Dl.(dlopen ~filename:"clib/test_functions.so" ~flags:[RTLD_NOW])


(*
  Check that we can store a reference to an OCaml function in a C global and
  invoke it later.
*)
let test_storing_function_reference () =
  let callback_type = int @-> returning int in
  let store_callback = foreign "store_callback" ~from:testlib
    (funptr callback_type @-> returning void) in
  let invoke_stored_callback = foreign "invoke_stored_callback" ~from:testlib
    (int @-> returning int) in

  (* This shouldn't be collected in the code that follows. *)
  let double x = x * 2 in

  begin
    store_callback double;
    Gc.major ();
    assert_equal 10 (invoke_stored_callback 5)
  end


(*
  Check that if a closure passed to C is collected before it's called then
  CallToExpiredClosure is raised.
  
  The value of this test is questionable: calling an expired closure does not
  have defined behaviour, since the structures needed to make the call may
  have been garbage collected.
*)
let test_calling_collected_closure_raises_exception () =
  let callback_type = int @-> returning int in
  let store_callback = foreign "store_callback" ~from:testlib
    (funptr callback_type @-> returning void) in
  let invoke_stored_callback = foreign "invoke_stored_callback" ~from:testlib
    (int @-> returning int) in

  let closure x y = x * y in

  begin
    (* The closure should be collected in the next GC *)
    store_callback (closure 2);
    (* The first GC collects the closure itself, which frees the associated object
       to be collected on the next GC. *)
    Gc.major ();
    Gc.major (); 
    assert_raises CallToExpiredClosure
      (fun () -> invoke_stored_callback 5)
  end


(*
  Check that we have fairly fine-grained control over the lifetime of closures
  passed to C.
*)
let test_controlling_closure_lifetime () =
  let callback_type = int @-> returning int in

  let return_callback = foreign "return_callback" ~from:testlib
    (funptr callback_type @-> returning (funptr callback_type)) in

  (* The return_callback function simply returns its argument.  However, since
     that involves converting an OCaml function ("arg") to a C function
     pointer and back to an OCaml function ("ret"), there are potential
     problems with memory management.  More precisely, ret holds a reference
     to a C/libffi closure, which in turn holds a reference to arg that is not
     visible to the GC.  We'd like to ensure that arg is not collected before
     ret is called, which requires that we store ret and arg together.  This
     test demonstrate the behaviour of naive and more careful
     implementations.  *)
  let module Sig = struct
    module type S =
    sig
      type t
      val make : arg:(int -> int) -> t
      val get : t -> (int -> int)
    end
  end in

  let module Naive : Sig.S = struct
    type t = {
      ret : int -> int ;
    }
    let make ~arg = { ret = return_callback arg }
    let get { ret } = ret
  end in

  let module Better : Sig.S = struct
    type t = {
      ret : int -> int ;
      arg : int -> int ;
    }
    let make ~arg = { arg ; ret = return_callback arg }
    let get { ret } = ret
  end in

  let module Careful : Sig.S = struct
    type t = {
      ret : int -> int ;
      arg : int -> int ;
    }
    let make ~arg = { arg ; ret = return_callback arg }
    let get { ret } c = ret c
  end in

  let closure x y = x * y in

  (* First, the naive implementation.  This should fail, because arg is
     collected before ret is called. *)
  let ret = Naive.make ~arg:(closure 3) in
  Gc.major ();
  assert_raises CallToExpiredClosure
    (fun () -> Naive.get ret 5);

  (* Now a more careful implementation.  This succeeds, because we keep a
     reference to arg around with the reference to ret *)
  let ret = Better.make ~arg:(closure 3) in
  Gc.major ();
  assert_equal 15 (Better.get ret 5);

  (* However, even with the careful implementation things can go wrong if we
     keep a reference to ret beyond the lifetime of the pair. *)
  let ret = Better.get (Better.make ~arg:(closure 3)) in
  Gc.major ();
  assert_raises CallToExpiredClosure
    (fun () -> ret 5);

  (* The most careful implementation calls ret rather than returning it,
     so arg cannot be collected prematurely. *)
  let ret = Careful.get (Careful.make ~arg:(closure 3)) in
  Gc.major ();
  assert_equal 15 (ret 5)


let suite = "Callback lifetime tests" >:::
  ["storing references to OCaml functions"
    >:: test_storing_function_reference;
   
   "calling expired closures"
    >:: test_calling_collected_closure_raises_exception;

   "controlling the lifetime of closures passed to C"
    >:: test_controlling_closure_lifetime;
  ]


let _ =
  run_test_tt_main suite
