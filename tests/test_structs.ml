open OUnit
open Ffi


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
    let simple : simple structure typ = structure "simple"
    let c = simple *:* int
    let f = simple *:* double
    let p = simple *:* ptr simple
    let () = seals simple
      
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

    let simple : simple structure typ = structure "simple"
    let c = simple *:* int
    let f = simple *:* double
    let p = simple *:* ptr simple
    let () = seals simple

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

    let styp : s structure typ = structure "s"
    let i = styp *:* int
    let j = styp *:* int
    let k = styp *:* ptr int
    let () = seals styp

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


(*
  Test structs with union members.
*)
let test_structs_with_union_members () =
  let module M = struct
    open Type
    type u and s

    open Union
    let utyp : u union typ = union "u"
    let uc = utyp +:+ char
    let ui = utyp +:+ int
    let uf = utyp +:+ double
    let () = sealu utyp

    let u = make utyp
    let () = begin
      setf u ui 14;
      assert_equal ~msg:"u.ui = 14" ~printer:string_of_int
        14 (getf u ui);

      setf u uc 'x';
      assert_equal ~msg:"u.uc = 'x'" ~printer:(String.make 1)
        'x' (getf u uc);

      setf u uf 5.55;
      assert_equal ~msg:"u.uf = 5.5" ~printer:string_of_float
        5.55 (getf u uf);
    end

    open Struct
    let styp : s structure typ = structure "s"
    let si = styp *:* int
    let su = styp *:* utyp
    let sc = styp *:* char
    let () = seals styp

    let s = make styp

    let () = begin
      setf s si 22;
      setf s su u;
      setf s sc 'z';

      assert_equal ~msg:"s.si = 22" ~printer:string_of_int
        22 (getf s si);
      
      assert_equal ~msg:"s.su.uc = 5.55" ~printer:string_of_float
        5.55 (Union.getf (getf s su) uf);

      assert_equal ~msg:"s.sc = 'z'" ~printer:(String.make 1)
        'z' (getf s sc);
    end
  end in ()



(*
  Test structs with array members.
*)
let test_structs_with_array_members () =
  let module M = struct
    open Type
    type u and s

    open Struct
    let styp : s structure typ = structure "s"
    let i = styp *:* int
    let a = styp *:* array 3 double
    let c = styp *:* char
    let () = seals styp

    let s = make styp

    let arr = Array.of_list double [3.3; 4.4; 5.5]

    let () = begin
      setf s i 22;
      setf s a arr;
      setf s c 'z';

      assert_equal ~msg:"s.i = 22" ~printer:string_of_int
        22 (getf s i);
      
      assert_equal ~msg:"s.a[0] = 3.3" ~printer:string_of_float
        3.3 (getf s a).(0);

      assert_equal ~msg:"s.a[0] = 3.3" ~printer:string_of_float
        3.3 (getf s a).(0);

      assert_equal ~msg:"s.a[1] = 4.4" ~printer:string_of_float
        4.4 (getf s a).(1);

      assert_equal ~msg:"s.a[2] = 5.5" ~printer:string_of_float
        5.5 (getf s a).(2);

      assert_raises (Invalid_argument "index out of bounds")
        (fun () -> (getf s a).(3));

      assert_equal ~msg:"s.c = 'z'" ~printer:(String.make 1)
        'z' (getf s c);

      (* References to the array member should alias the original *)
      let arr' = getf s a in
      
      arr'.(0) <- 13.3;
      arr'.(1) <- 24.4;
      arr'.(2) <- 35.5;

      assert_equal ~msg:"s.a[0] = 13.3" ~printer:string_of_float
        13.3 (getf s a).(0);

      assert_equal ~msg:"s.a[1] = 24.4" ~printer:string_of_float
        24.4 (getf s a).(1);

      assert_equal ~msg:"s.a[2] = 35.5" ~printer:string_of_float
        35.5 (getf s a).(2);
    end
  end in ()


let suite = "Struct tests" >:::
  ["passing struct" >:: test_passing_struct;
   "returning struct" >:: test_returning_struct;
   "pointers to struct members" >:: test_pointers_to_struct_members;
   "structs with union members" >:: test_structs_with_union_members;
   "structs with array members" >:: test_structs_with_array_members;
  ]


let _ =
  run_test_tt_main suite
