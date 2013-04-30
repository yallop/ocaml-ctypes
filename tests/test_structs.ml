open OUnit
open Ffi.C


let testlib = Dl.(dlopen ~filename:"clib/test_functions.so" ~flags:[RTLD_NOW])


(*
  Call a function of type

     void (struct simple)

  where

     struct simple {
       int i;
       double f;
       struct simple *self;
     };
*)
let test_passing_struct () =
  let module M = struct
    open Struct
    open Type
    type simple
    let simple : simple structure typ = tag "simple"
    let c = simple *:* int
    let f = simple *:* double
    let p = simple *:* ptr simple
    let () = seal simple
      
    let accept_struct = foreign "accept_struct" (simple @-> returning int)
      ~from:testlib
      
    let s = make simple

    let () = Ptr.(begin
      setf s c 10;
      setf s f 14.5;
      setf s p (from_voidp simple null)
    end)
      
    let v = accept_struct s

    let () = assert_equal 25 v
      ~printer:string_of_int

  end in ()


(*
  Call a function of type

     struct simple(void)

  where

     struct simple {
       int i;
       double f;
       struct simple *self;
     };
*)
let test_returning_struct () =
  let module M = struct
    open Struct
    open Type
    type simple

    let simple : simple structure typ = tag "simple"
    let c = simple *:* int
    let f = simple *:* double
    let p = simple *:* ptr simple
    let () = seal simple

    let return_struct = foreign "return_struct" (void @-> returning simple)
      ~from:testlib

    let s = return_struct ()

    let () = assert_equal 20 (getf s c)
    let () = assert_equal 35.0 (getf s f)

    let t = getf s p

    let () = assert_equal 10 Ptr.(!(t |-> c))
      ~printer:string_of_int
    let () = assert_equal 12.5 Ptr.(!(t |-> f))
      ~printer:string_of_float

    let () = prerr_endline "TODO: reinstate once we have pointer equality sorted out"
    (* let () = assert_equal Ptr.(!(!t --> p)) t *)

  end in ()


let suite = "Struct tests" >:::
  ["passing struct" >:: test_passing_struct;
   "returning struct" >:: test_returning_struct;
  ]


let _ =
  run_test_tt_main suite
