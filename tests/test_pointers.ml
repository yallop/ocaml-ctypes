(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

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
     accept_pointers
       (allocate float 1.0)
       (allocate double 2.0)
       (allocate short 3)
       (allocate int 4)
       (allocate long (Long.of_int 5))
       (allocate llong (LLong.of_int 6))
       (allocate nativeint 7n)
       (allocate int8_t 8)
       (allocate int16_t 9)
       (allocate int32_t 10l)
       (allocate int64_t 11L)
       (allocate uint8_t (UInt8.of_int 12))
       (allocate uint16_t (UInt16.of_int 13))
       (allocate uint32_t (UInt32.of_int 14))
       (allocate uint64_t (UInt64.of_int 15))
       (allocate size_t (Size_t.of_int 16))
       (allocate ushort (UShort.of_int 17))
       (allocate uint (UInt.of_int 18))
       (allocate ulong (ULong.of_int 19))
       (allocate ullong (ULLong.of_int 20)))


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

  let p = allocate int 1
  and pp = allocate (ptr int) (allocate int 2)
  and ppp = allocate (ptr (ptr int)) (allocate (ptr int) (allocate int 3))
  and pppp = allocate (ptr (ptr (ptr int)))
    (allocate (ptr (ptr int)) (allocate (ptr int) (allocate int 4))) in

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
  assert_equal 7
    (passing_pointers_to_callback (fun lp rp -> !@lp + !@rp))


(*
  Passing a callback that returns a pointer.
*)
let test_callback_returning_pointers () =
  let pintfun2 = int @-> int @-> returning (ptr int) in
  let p = allocate int 17 in
  let accepting_pointer_from_callback =
    foreign ~from:testlib "accepting_pointer_from_callback"
      (funptr pintfun2 @-> returning int)
  in begin
    assert_equal 17 !@p;

    assert_equal 56
      (accepting_pointer_from_callback (fun x y -> p <-@ (x * y); p));

    assert_equal 12 !@p
  end


(*
  Tests for reading and writing primitive values through pointers.
*)
let test_pointer_assignment_with_primitives () =
  let open Signed in
  let open Unsigned in
  let p_char = allocate char '1'
  and p_uchar = allocate uchar (UChar.of_int 2)
  and p_schar = allocate schar 3
  and p_float = allocate float 4.0
  and p_double = allocate double 5.0
  and p_short = allocate short 6
  and p_int = allocate int 7
  and p_long = allocate long (Long.of_int 8)
  and p_llong = allocate llong (LLong.of_int 9)
  and p_nativeint = allocate nativeint 10n
  and p_int8_t = allocate int8_t 11
  and p_int16_t = allocate int16_t 12
  and p_int32_t = allocate int32_t 13l
  and p_int64_t = allocate int64_t 14L
  and p_uint8_t = allocate uint8_t (UInt8.of_int 15)
  and p_uint16_t = allocate uint16_t (UInt16.of_int 16)
  and p_uint32_t = allocate uint32_t (UInt32.of_int 17)
  and p_uint64_t = allocate uint64_t (UInt64.of_int 18)
  and p_size_t = allocate size_t (Size_t.of_int 19)
  and p_ushort = allocate ushort (UShort.of_int 20)
  and p_uint = allocate uint (UInt.of_int 21)
  and p_ulong = allocate ulong (ULong.of_int 22)
  and p_ullong = allocate ullong (ULLong.of_int 23)
  in begin
    assert_equal '1' (!@p_char);
    assert_equal (UChar.of_int 2) (!@p_uchar);
    assert_equal 3 (!@p_schar);
    assert_equal 4.0 (!@p_float);
    assert_equal 5.0 (!@p_double);
    assert_equal 6 (!@p_short);
    assert_equal 7 (!@p_int);
    assert_equal (Long.of_int 8) (!@p_long);
    assert_equal (LLong.of_int 9) (!@p_llong);
    assert_equal 10n (!@p_nativeint);
    assert_equal 11 (!@p_int8_t);
    assert_equal 12 (!@p_int16_t);
    assert_equal 13l (!@p_int32_t);
    assert_equal 14L (!@p_int64_t);
    assert_equal (UInt8.of_int 15) (!@p_uint8_t);
    assert_equal (UInt16.of_int 16) (!@p_uint16_t);
    assert_equal (UInt32.of_int 17) (!@p_uint32_t);
    assert_equal (UInt64.of_int 18) (!@p_uint64_t);
    assert_equal (Size_t.of_int 19) (!@p_size_t);
    assert_equal (UShort.of_int 20) (!@p_ushort);
    assert_equal (UInt.of_int 21) (!@p_uint);
    assert_equal (ULong.of_int 22) (!@p_ulong);
    assert_equal (ULLong.of_int 23) (!@p_ullong);

    p_char <-@ '2';
    p_uchar <-@ (UChar.of_int 102);
    p_schar <-@ 103;
    p_float <-@ 104.0;
    p_double <-@ 105.0;
    p_short <-@ 106;
    p_int <-@ 107;
    p_long <-@ (Long.of_int 108);
    p_llong <-@ (LLong.of_int 109);
    p_nativeint <-@ 110n;
    p_int8_t <-@ 111;
    p_int16_t <-@ 112;
    p_int32_t <-@ 113l;
    p_int64_t <-@ 114L;
    p_uint8_t <-@ (UInt8.of_int 115);
    p_uint16_t <-@ (UInt16.of_int 116);
    p_uint32_t <-@ (UInt32.of_int 117);
    p_uint64_t <-@ (UInt64.of_int 118);
    p_size_t <-@ (Size_t.of_int 119);
    p_ushort <-@ (UShort.of_int 120);
    p_uint <-@ (UInt.of_int 121);
    p_ulong <-@ (ULong.of_int 122);
    p_ullong <-@ (ULLong.of_int 123);

    assert_equal '2' (!@p_char);
    assert_equal (UChar.of_int 102) (!@p_uchar);
    assert_equal 103 (!@p_schar);
    assert_equal 104.0 (!@p_float);
    assert_equal 105.0 (!@p_double);
    assert_equal 106 (!@p_short);
    assert_equal 107 (!@p_int);
    assert_equal (Long.of_int 108) (!@p_long);
    assert_equal (LLong.of_int 109) (!@p_llong);
    assert_equal 110n (!@p_nativeint);
    assert_equal 111 (!@p_int8_t);
    assert_equal 112 (!@p_int16_t);
    assert_equal 113l (!@p_int32_t);
    assert_equal 114L (!@p_int64_t);
    assert_equal (UInt8.of_int 115) (!@p_uint8_t);
    assert_equal (UInt16.of_int 116) (!@p_uint16_t);
    assert_equal (UInt32.of_int 117) (!@p_uint32_t);
    assert_equal (UInt64.of_int 118) (!@p_uint64_t);
    assert_equal (Size_t.of_int 119) (!@p_size_t);
    assert_equal (UShort.of_int 120) (!@p_ushort);
    assert_equal (UInt.of_int 121) (!@p_uint);
    assert_equal (ULong.of_int 122) (!@p_ulong);
    assert_equal (ULLong.of_int 123) (!@p_ullong);
  end


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
         (allocate arg_type ( / )))



(*
  Test returning a pointer to a function pointer
*)
let test_callback_returning_pointer_to_function_pointer () =
  let returning_pointer_to_function_pointer =
    foreign "returning_pointer_to_function_pointer" ~from:testlib
      (void @-> returning (ptr (funptr (int @-> int @-> returning int))))
  in
  assert_equal
    10 (!@(returning_pointer_to_function_pointer ()) 2 5)


(*
  Dereferencing pointers to incomplete types
*)
let test_dereferencing_pointers_to_incomplete_types () =
  begin
    assert_raises IncompleteType
      (fun () -> !@null);

    assert_raises IncompleteType
      (fun () -> !@(from_voidp (structure "incomplete") null));

    assert_raises IncompleteType
      (fun () -> !@(from_voidp (union "incomplete") null));
  end


(*
  Writing through a pointer to an abstract type
*)
let test_writing_through_pointer_to_abstract_type () =
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

    dest <-@ !@src;

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
let test_reading_and_writing_global_value () =
  let ptr = foreign_value "global" int ~from:testlib in
  let ptr' = foreign_value "global" int ~from:testlib in
  assert_equal (!@ptr) 100;
  ptr <-@ 200;
  assert_equal (!@ptr) 200;
  assert_equal (!@ptr') 200;
  ptr' <-@ 100;
  assert_equal (!@ptr) 100;
  assert_equal (!@ptr') 100


(*
  Test bindings for malloc, realloc and free.
*)
let test_allocation () =
  let open Unsigned in
  let malloc = foreign "malloc" (size_t @-> returning (ptr void)) in
  let realloc = foreign "realloc" (ptr void @-> size_t @-> returning (ptr void)) in
  let free = foreign "free" (ptr void @-> returning void) in
  
  let pointer = malloc (Size_t.of_int (sizeof int)) in
    let int_pointer = from_voidp int pointer in
    int_pointer <-@ 17;
    assert_equal !@int_pointer 17;
    int_pointer <-@ -3;
    assert_equal !@int_pointer (-3);

    let pointer' = realloc pointer (Size_t.of_int (20 * sizeof int)) in
    assert_bool "realloc succeeded" (pointer' <> null);
    let int_pointer = from_voidp int pointer' in

    assert_equal ~msg:"realloc copied the existing data over"
      !@int_pointer (-3);

    for i = 0 to 19 do
      (int_pointer +@ i) <-@ i
    done;

    for i = 0 to 19 do
      assert_equal i !@(int_pointer +@ i)
    done;

    free pointer'


(*
  Test a function that returns the address of a global variable.
*)
let test_reading_returned_global () =
  let return_global_address = 
    foreign "return_global_address" ~from:testlib
      (void @-> returning (ptr int)) in
  assert_equal (!@(return_global_address ())) 100


(*
  Test a function that returns a pointer passed as argument.
*)
let test_passing_pointer_through () =
  let pass_pointer_through = foreign "pass_pointer_through" ~from:testlib
    (ptr int @-> ptr int @-> int @-> returning (ptr int)) 
  in
  let p1 = allocate int 25 in
  let p2 = allocate int 32 in
  let rv = pass_pointer_through p1 p2 10 in
  assert_equal !@rv !@p1;
  assert_equal 25 !@rv;
  let rv = pass_pointer_through p1 p2 (-10) in
  assert_equal !@rv !@p2;
  assert_equal 32 !@rv


(*
  Tests for various aspects of pointer arithmetic.
*)
let test_pointer_arithmetic () =
  let arr = Array.of_list int [1;2;3;4;5;6;7;8] in

  (* Traverse the array using an int pointer *)
  let p = Array.start arr in
  for i = 0 to 7 do
    assert_equal !@(p +@ i) (succ i)
  done;

  let twoints = structure "s" in
  let i1 = twoints *:* int in
  let i2 = twoints *:* int in
  let () = seals twoints in
  
  (* Traverse the array using a 'struct twoints' pointer *)
  let ps = from_voidp twoints (to_voidp p) in

  for i = 0 to 3 do
    assert_equal !@((ps +@ i) |-> i1) (2 * i + 1);
    assert_equal !@((ps +@ i) |-> i2) (2 * i + 2);
  done;
  
  (* Traverse the array using a char pointer *)
  let pc = from_voidp char (to_voidp p) in
  
  for i = 0 to 7 do
    let p' = pc +@ i * sizeof int in
    assert_equal !@(from_voidp int (to_voidp p')) (succ i)
  done;

  (* Reverse traversal *)
  let pend = p +@ 7 in
  for i = 0 to 7 do
    assert_equal !@(pend -@ i) (8 - i)
  done
    

(*
  Test pointer comparisons.
*)
let test_pointer_comparison () =
  let canonicalize p =
    (* Ensure that the 'pbyte_offset' component of the pointer is zero by
       writing the pointer to memory and then reading it back. *)
    let buf = allocate_n ~count:1 (ptr void) in
    buf <-@ (to_voidp p);
    !@buf
  in

  let (<) l r = ptr_compare l r < 0
  and (>) l r = ptr_compare l r > 0
  and (=) l r = ptr_compare l r = 0 in

  (* equal but not identical pointers compare equal *)
  let p = allocate int 10 in
  let p' = from_voidp int (to_voidp p) in
  assert_bool "equal but not identical poitners compare equal"
    (p = p');

  (* Canonicalization preserves ordering *)
  assert_bool "p < p+n"
    (p < (p +@ 10));
    
  assert_bool "canonicalize(p) < canonicalize(p+n)"
    (canonicalize p < canonicalize (p +@ 10));

  assert_bool "p > p-1"
    (p > (p -@ 1));
    
  assert_bool "canonicalize(p) > canonicalize(p-1)"
    (canonicalize p > canonicalize (p -@ 1));

  let s3 = structure "s3" in
  let i = s3 *:* int in
  let j = s3 *:* int in
  let k = s3 *:* int in
  let () = seals s3 in

  let sp = addr (make s3) in
  let p1 = to_voidp (sp |-> i)
  and p2 = to_voidp (sp |-> j)
  and p3 = to_voidp (sp |-> k) in

  assert_bool "sp |-> i < sp |-> j"
    (p1 < p2);

  assert_bool "sp |-> i < canonicalize (sp |-> j)"
    (p1 < canonicalize p2);

  assert_bool "canonicalize (sp |-> i) < sp |-> j"
    (canonicalize p1 < p2);

  assert_bool "canonicalize (sp |-> i) < canonicalize (sp |-> j)"
    (canonicalize p1 < canonicalize p2);

  assert_bool "sp |-> i < sp |-> k"
    (p1 < p3);

  assert_bool "sp |-> i < canonicalize (sp |-> k)"
    (p1 < canonicalize p3);

  assert_bool "canonicalize (sp |-> i) < sp |-> k"
    (canonicalize p1 < p3);

  assert_bool "canonicalize (sp |-> i) < canonicalize (sp |-> k)"
    (canonicalize p1 < canonicalize p3);

  assert_bool "sp |-> j < sp |-> k"
    (p2 < p3);

  assert_bool "sp |-> j < canonicalize (sp |-> k)"
    (p2 < canonicalize p3);

  assert_bool "canonicalize (sp |-> j) < sp |-> k"
    (canonicalize p2 < p3);

  assert_bool "canonicalize (sp |-> j) < canonicalize (sp |-> k)"
    (canonicalize p2 < canonicalize p3);

  (* Canonicalization preserves equality *)
  assert_bool "canonicalization preserves equality"
    (to_voidp p = canonicalize p)


let suite = "Pointer tests" >:::
  ["passing pointers"
    >:: test_passing_pointers;

   "passing pointers to pointers"
    >:: test_passing_pointers_to_pointers;

   "callback receiving pointers"
    >:: test_callback_receiving_pointers;

   "callback returning pointers"
    >:: test_callback_returning_pointers;

   "pointer assignment with primitives"
    >:: test_pointer_assignment_with_primitives;

   "passing pointer to function pointer"
    >:: test_passing_pointer_to_function_pointer;

   "callback returning pointer to function pointer"
    >:: test_callback_returning_pointer_to_function_pointer;

   "incomplete types"
    >:: test_dereferencing_pointers_to_incomplete_types;

   "abstract types"
    >:: test_writing_through_pointer_to_abstract_type;

   "global value"
    >:: test_reading_and_writing_global_value;

   "allocation"
    >:: test_allocation;

   "passing pointers through functions"
    >:: test_passing_pointer_through;

   "returned globals"
    >:: test_reading_returned_global;

   "arithmetic"
    >:: test_pointer_arithmetic;

   "comparisons"
    >:: test_pointer_comparison;
  ]


let _ =
  run_test_tt_main suite
