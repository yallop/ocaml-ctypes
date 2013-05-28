open OUnit
open Ctypes


let testlib = Dl.(dlopen ~filename:"clib/test_functions.so" ~flags:[RTLD_NOW])


(*
  Test passing various types of pointers to a function.
*)
let test_passing_pointers () =
  let accept_pointers = foreign "accept_pointers" ~from:testlib
    (ptr float @->
     ptr double @->
     ptr short @->
     ptr int @->
     ptr long @->
     ptr llong @->
     ptr nativeint @->
     ptr int8_t @->
     ptr int16_t @->
     ptr int32_t @->
     ptr int64_t @->
     ptr uint8_t @->
     ptr uint16_t @->
     ptr uint32_t @->
     ptr uint64_t @->
     ptr size_t @->
     ptr ushort @->
     ptr uint @->
     ptr ulong @->
     ptr ullong @->
     returning int) in
  assert_equal ~msg:"Passing pointers to various numeric types"
    ~printer:string_of_int
    (1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10 +
     11 + 12 + 13 + 14 + 15 + 16 + 17 + 18 + 19 + 20)
    (let open Signed in
     let open Unsigned in
     let open Ptr in
     accept_pointers
       (make float 1.0)
       (make double 2.0)
       (make short 3)
       (make int 4)
       (make long (Long.of_int 5))
       (make llong (LLong.of_int 6))
       (make nativeint 7n)
       (make int8_t 8)
       (make int16_t 9)
       (make int32_t 10l)
       (make int64_t 11L)
       (make uint8_t (Uint8.of_int 12))
       (make uint16_t (Uint16.of_int 13))
       (make uint32_t (Uint32.of_int 14))
       (make uint64_t (Uint64.of_int 15))
       (make size_t (Size_t.of_int 16))
       (make ushort (UShort.of_int 17))
       (make uint (UInt.of_int 18))
       (make ulong (ULong.of_int 19))
       (make ullong (ULLong.of_int 20)))


(*
  Test passing pointers to pointers.
*)
let test_passing_pointers_to_pointers () =
  let accept_pointers_to_pointers =
    foreign "accept_pointers_to_pointers" ~from:testlib
      (ptr int @->
       ptr (ptr int) @->
       ptr (ptr (ptr int)) @->
       ptr (ptr (ptr (ptr int))) @->
       returning int) in

  let open Ptr in
  let p = make int 1
  and pp = make (ptr int) (make int 2)
  and ppp = make (ptr (ptr int)) (make (ptr int) (make int 3))
  and pppp = make (ptr (ptr (ptr int)))
    (make (ptr (ptr int)) (make (ptr int) (make int 4))) in

  assert_equal ~msg:"Passing pointers to pointers"
    Pervasives.(1 + 2 + 3 + 4)
    (accept_pointers_to_pointers p pp ppp pppp)


(*
  Passing a callback that accepts pointers as arguments.
*)
let test_callback_receiving_pointers () =
  let pintfun1 = ptr int @-> ptr int @-> returning int in
  let passing_pointers_to_callback =
    foreign ~from:testlib "passing_pointers_to_callback"
      (funptr pintfun1 @-> returning int)
  in
  let deref = Ptr.(!) in
  assert_equal 7
    (passing_pointers_to_callback (fun lp rp -> deref lp + deref rp))


(*
  Passing a callback that returns a pointer.
*)
let test_callback_returning_pointers () =
  let pintfun2 = int @-> int @-> returning (ptr int) in
  let p = Ptr.make int 17 in
  let accepting_pointer_from_callback =
    foreign ~from:testlib "accepting_pointer_from_callback"
      (funptr pintfun2 @-> returning int)
  in begin
    assert_equal 17 Ptr.(!p);

    assert_equal 56
      (accepting_pointer_from_callback Ptr.(fun x y -> p := (x * y); p));

    assert_equal 12 Ptr.(!p)
  end


(*
  [TODO]
*)
let test_pointer_assignment_with_primitives () =
  () (* TODO *)


(*
  Test passing a pointer-to-a-function-pointer as an argument.
*)
let test_passing_pointer_to_function_pointer () =
  let arg_type = funptr (int @-> int @-> returning int) in
  let accepting_pointer_to_function_pointer =
    foreign "accepting_pointer_to_function_pointer" ~from:testlib
      (ptr arg_type @-> returning int)
  in
  assert_equal ~printer:string_of_int
    5 (accepting_pointer_to_function_pointer 
         (Ptr.make arg_type ( / )))



(*
  Test returning a pointer to a function pointer
*)
let test_callback_returning_pointer_to_function_pointer () =
  let returning_pointer_to_function_pointer =
    foreign "returning_pointer_to_function_pointer" ~from:testlib
      (void @-> returning (ptr (funptr (int @-> int @-> returning int))))
  in
  assert_equal
    10 Ptr.(!(returning_pointer_to_function_pointer ()) 2 5)


(*
  Dereferencing pointers to incomplete types
*)
let test_dereferencing_pointers_to_incomplete_types () =
  let open Ptr in begin
    assert_raises IncompleteType
      (fun () -> !null);

    assert_raises IncompleteType
      (fun () -> !(from_voidp (Struct.structure "incomplete") null));

    assert_raises IncompleteType
      (fun () -> !(from_voidp (Union.union "incomplete") null));
  end


(*
  Writing through a pointer to an abstract type
*)
let test_writing_through_pointer_to_abstract_type () =
  let open Ptr in
  let arra = Array.make int 2 in
  let arrb = Array.make int 2 in
  let absptr a =
    from_voidp (abstract
                  ~size:(2 * sizeof int)
                  ~alignment:(alignment (array 2 int)))
      (to_voidp (Array.start a)) in
  let () = begin
    arra.(0) <- 10;
    arra.(1) <- 20;
    arrb.(0) <- 30;
    arrb.(1) <- 40;
  end in
  let dest = absptr arra in
  let src = absptr arrb in
  begin
    assert_equal 10 arra.(0);
    assert_equal 20 arra.(1);
    assert_equal 30 arrb.(0);
    assert_equal 40 arrb.(1);

    dest := !src;

    assert_equal 30 arra.(0);
    assert_equal 40 arra.(1);
    assert_equal 30 arrb.(0);
    assert_equal 40 arrb.(1);

    assert_bool "pointers distinct" (dest <> src);

    assert_bool "arrays distinct" (arra <> arrb);
  end


(* 
   Test for reading and writing global values using the "foreign_value"
   function.
*)
let test_reading_and_writing_global_value () = Ptr.(
  let ptr = foreign_value "global" int
    ~from:testlib in
  let ptr' = foreign_value "global" int
    ~from:testlib in
  assert_equal (!ptr) 100;
  ptr := 200;
  assert_equal (!ptr) 200;
  assert_equal (!ptr') 200;
  ptr' := 100;
  assert_equal (!ptr) 100;
  assert_equal (!ptr') 100;
)


(*
  Test bindings for malloc, realloc and free.
*)
let test_allocation () =
  let open Unsigned in
  let malloc = foreign "malloc" (size_t @-> returning (ptr void)) in
  let realloc = foreign "realloc" (ptr void @-> size_t @-> returning (ptr void)) in
  let free = foreign "free" (ptr void @-> returning void) in
  
  let pointer = malloc (Size_t.of_int (sizeof int)) in Ptr.(
    let int_pointer = from_voidp int pointer in
    int_pointer := 17;
    assert_equal !int_pointer 17;
    int_pointer := -3;
    assert_equal !int_pointer (-3);

    let pointer' = realloc pointer (Size_t.of_int (20 * sizeof int)) in
    assert_bool "realloc succeeded" (pointer' <> null);
    let int_pointer = from_voidp int pointer' in

    assert_equal ~msg:"realloc copied the existing data over"
      !int_pointer (-3);

    for i = 0 to 19 do
      (int_pointer + i) := i
    done;

    for i = 0 to 19 do
      assert_equal i !(int_pointer + i)
    done;

    free pointer'
  )


(*
  Test a function that returns the address of a global variable.
*)
let test_reading_returned_global () =
  let return_global_address = 
    foreign "return_global_address" (void @-> returning (ptr int)) 
      ~from:testlib in
  Ptr.(assert_equal (!(return_global_address ())) 100)


(*
  Test a function that returns a pointer passed as argument.
*)
let test_passing_pointer_through () =
  let open Ptr in
  let pass_pointer_through = 
    foreign "pass_pointer_through" (ptr int @-> ptr int @-> int @-> returning (ptr int)) 
      ~from:testlib in
  let p1 = Ptr.make int 25 in
  let p2 = Ptr.make int 32 in
  let rv = pass_pointer_through p1 p2 10 in
  assert_equal !rv !p1;
  assert_equal 25 !rv;
  let rv = pass_pointer_through p1 p2 (-10) in
  assert_equal !rv !p2;
  assert_equal 32 !rv


(*
  [TODO]
*)
let test_pointer_arithmetic () =
  (* TODO *)
  ()


let suite = "Pointer tests" >:::
  ["passing pointers" >:: test_passing_pointers;
   "passing pointers to pointers" >:: test_passing_pointers_to_pointers;
   "callback receiving pointers" >:: test_callback_receiving_pointers;
   "callback returning pointers" >:: test_callback_returning_pointers;
   "pointer assignment with primitives" >:: test_pointer_assignment_with_primitives;
   "passing pointer to function pointer" >:: test_passing_pointer_to_function_pointer;
   "callback returning pointer to function pointer" >:: test_callback_returning_pointer_to_function_pointer;
   "incomplete types" >:: test_dereferencing_pointers_to_incomplete_types;
   "abstract types" >:: test_writing_through_pointer_to_abstract_type;
   "global value" >:: test_reading_and_writing_global_value;
   "allocation" >:: test_allocation;
   "passing pointers through functions" >:: test_passing_pointer_through;
   "returned globals" >:: test_reading_returned_global;
   "arithmetic" >:: test_pointer_arithmetic;
  ]


let _ =
  run_test_tt_main suite
