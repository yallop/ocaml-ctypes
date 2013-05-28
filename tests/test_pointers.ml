open OUnit
open Ctypes


let testlib = Dl.(dlopen ~filename:"clib/test_functions.so" ~flags:[RTLD_NOW])


(*
  [TODO]
*)
let test_passing_pointers () =
  () (* TODO *)


(*
  [TODO]
*)
let test_passing_pointers_to_pointers () =
  () (* TODO *)


(*
  [TODO]
*)
let test_callback_receiving_pointers () =
  () (* TODO *)


(*
  [TODO]
*)
let test_callback_returning_pointers () =
  () (* TODO *)


(*
  [TODO]
*)
let test_pointer_assignment_with_primitives () =
  () (* TODO *)


(*
  [TODO]
*)
let test_passing_pointer_to_function_pointer () =
  () (* TODO *)


(*
  [TODO]
*)
let test_callback_returning_pointer_to_function_pointer () =
  () (* TODO *)


(*
  [TODO]
*)
let test_returning_pointer_to_void () =
  () (* TODO *)


(*
  [TODO]
*)
let test_passing_pointer_to_void () =
  () (* TODO *)


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
  [TODO]
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
  [TODO]
*)
let test_allocation () =
  let malloc = foreign "malloc" (int @-> returning (ptr void)) in
  let free = foreign "free" (ptr void @-> returning void) in
  
  let pointer = malloc (sizeof int) in Ptr.(
    let int_pointer = from_voidp int pointer in
    int_pointer := 17;
    assert_equal !int_pointer 17;
    int_pointer := -3;
    assert_equal !int_pointer (-3);
    free pointer
  )


(*
  [TODO]
*)
let test_reading_returned_global () =
  let return_global_address = 
    foreign "return_global_address" (void @-> returning (ptr int)) 
      ~from:testlib in
  Ptr.(assert_equal (!(return_global_address ())) 100)


(*
  [TODO]
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
   "passing_pointers_to_pointers" >:: test_passing_pointers_to_pointers;
   "callback_receiving_pointers" >:: test_callback_receiving_pointers;
   "callback_returning_pointers" >:: test_callback_returning_pointers;
   "pointer_assignment_with_primitives" >:: test_pointer_assignment_with_primitives;
   "passing_pointer_to_function_pointer" >:: test_passing_pointer_to_function_pointer;
   "callback_returning_pointer_to_function_pointer" >:: test_callback_returning_pointer_to_function_pointer;
   "returning_pointer_to_void" >:: test_returning_pointer_to_void;
   "passing_pointer_to_void" >:: test_passing_pointer_to_void;

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
