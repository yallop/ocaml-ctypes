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

    let () = assert_equal Ptr.(!(t |-> p)) t

  end in ()


(*
  Test reading and writing pointers to struct members.
*)
let test_pointers_to_struct_members () =
  let module M = struct
    open Type
    open Struct
    type s

    let styp : s structure typ = tag "s"
    let i = styp *:* int
    let j = styp *:* int
    let k = styp *:* ptr int
    let () = seal styp

    let s = make styp

    let () = Ptr.(begin
      let sp = addr s in
      sp |-> i := 10;
      sp |-> j := 20;
      sp |-> k := sp |-> i;
      assert_equal ~msg:"sp->i = 10" ~printer:string_of_int
        10 (!(sp |-> i));
      assert_equal ~msg:"sp->j = 20" ~printer:string_of_int
        20 (!(sp |-> j));
      assert_equal ~msg:"*sp->k = 10" ~printer:string_of_int
        10 (!(!(sp |-> k)));
      (sp |-> k) := (sp |-> j);
      assert_equal ~msg:"*sp->k = 20" ~printer:string_of_int
        20 (!(!(sp |-> k)));
      sp |-> i := 15;
      sp |-> j := 25;
      assert_equal ~msg:"*sp->k = 25" ~printer:string_of_int
        25 (!(!(sp |-> k)));
      sp |-> k := sp |-> i;
      assert_equal ~msg:"*sp->k = 15" ~printer:string_of_int
        15 (!(!(sp |-> k)));
    end)
  end in ()

let suite = "Struct tests" >:::
  ["passing struct" >:: test_passing_struct;
   "returning struct" >:: test_returning_struct;
   "pointers to struct members" >:: test_pointers_to_struct_members;
  ]


let _ =
  run_test_tt_main suite
