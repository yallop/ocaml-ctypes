open OUnit
open Ctypes


(*
  Test some relationships between the sizes of primitive types.
*)
let test_sizeof_primitives () = begin
  assert_equal ~msg:"sizeof (char) == 1"
    (sizeof char) 1;

  assert_equal ~msg:"sizeof (unsigned char) == 1"
    (sizeof uchar) 1;

  assert_equal ~msg:"sizeof (signed char) == 1"
    (sizeof schar) 1;

  assert_bool "sizeof (char) <= sizeof (int)"
    (sizeof char <= sizeof int);

  assert_bool "sizeof (float) <= sizeof (double)"
    (sizeof char <= sizeof int);

  assert_bool "sizeof (short) <= sizeof (int)"
    (sizeof char <= sizeof int);

  assert_bool "sizeof (int) <= sizeof (long)"
    (sizeof int <= sizeof long);

  assert_bool "sizeof (long) <= sizeof (long long)"
    (sizeof long <= sizeof llong);

  assert_equal ~msg:"2 * sizeof (int32_t) == sizeof (int64_t)"
    (2 * sizeof int32_t) (sizeof int64_t);

  assert_equal ~msg:"2 * sizeof (int16_t) == sizeof (int32_t)"
    (2 * sizeof int16_t) (sizeof int32_t);

  assert_equal ~msg:"2 * sizeof (int8_t) == sizeof (int16_t)"
    (2 * sizeof int8_t) (sizeof int16_t);

  assert_bool "sizeof (int16_t) <= sizeof (int)"
    (sizeof int16_t <= sizeof int);

  assert_bool "sizeof (int32_t) <= sizeof (long)"
    (sizeof int32_t <= sizeof long);

  assert_bool "sizeof (int64_t) <= sizeof (long long)"
    (sizeof int64_t <= sizeof llong);

  assert_equal ~msg:"sizeof (short) == sizeof (unsigned short)"
    (sizeof short) (sizeof ushort);

  assert_equal ~msg:"sizeof (int) == sizeof (unsigned int)"
    (sizeof int) (sizeof uint);

  assert_equal ~msg:"sizeof (long) == sizeof (unsigned long)"
    (sizeof long) (sizeof ulong);

  assert_equal ~msg:"sizeof (long long) == sizeof (unsigned long long)"
    (sizeof llong) (sizeof ullong);
end



(*
  Test some properties of the sizes of unions.
*)
let test_sizeof_unions () =
  let open Union in

  let int_char = union "int_char" in
  let _ = int_char +:+ int in
  let _ = int_char +:+ char in
  let _ = sealu int_char in
  
  assert_equal (sizeof int) (sizeof int_char);


  let char17 = union "char17" in
  let _ = char17 +:+ array 17 char in
  let _ = sealu char17 in
  
  assert_equal 17 (sizeof char17)


(*
  Test some properties of the sizes of structs.
*)
let test_sizeof_structs () =
  let module M = struct
    open Struct

    (* We don't expect homogeneous structs consisting of words to have
       any padding. *)
    type h
    let () =
      for i = 1 to 10 do
        let homogeneous : h structure typ = structure "h" in
        for j = 1 to i do
          ignore (homogeneous *:* int);
        done;
        seals homogeneous;
        assert_equal (i * sizeof int) (sizeof homogeneous)
      done

  end in ()
  

(*
  Test that taking the size of an incomplete type is treated as an error.
*)
let test_sizeof_incomplete () = begin
  assert_raises IncompleteType
    (fun () -> sizeof (Struct.structure "incomplete"));

  assert_raises IncompleteType
    (fun () -> sizeof (Union.union "incomplete"));
end
  

(*
  Test that taking the size of void is treated as an error.
*)
let test_sizeof_void () =
  assert_raises IncompleteType
    (fun () -> sizeof void)
 

(*
  Test the behaviour of sizeof on array types.
*)
let test_sizeof_arrays () = begin
  assert_equal ~msg:"The size of an array is the sum of the size of its members"
    (12 * (sizeof int8_t)) (sizeof (array 12 int8_t));

  assert_equal ~msg:"Arrays of arrays are correctly sized"
    (5 * 7 * (sizeof nativeint)) (sizeof (array 7 (array 5 nativeint)))
end
 

(*
  Test that all pointers have equal size.
*)
let test_sizeof_pointers () = begin
  let pointer_size = sizeof (ptr void) in
  assert_equal pointer_size (sizeof (ptr void));
  assert_equal pointer_size (sizeof (ptr int));
  assert_equal pointer_size (sizeof (funptr (int @-> returning int)));
  assert_equal pointer_size (sizeof (ptr (ptr void)));
  let module M = struct
    open Struct
    type t
    let t : t structure typ = structure "t"
    let c = t *:* int
    let f = t *:* double
    let () = seals t
  end in
  assert_equal pointer_size (sizeof (ptr M.t))
end


(*
  Test that the size of a view type is the same as the underlying type.
*)
let test_sizeof_views () = begin
  let const c x = c in
  let vint = view ~read:(const [1]) ~write:(const 0) int
  and vchar = view ~read:(const ["1"]) ~write:(const 'a') char
  and vvoid = view ~read:(const (fun () -> ())) ~write:(const ()) void
  in
  assert_equal (sizeof int) (sizeof vint);
  assert_equal (sizeof char) (sizeof vchar);
  assert_raises IncompleteType (fun () -> sizeof vvoid);
end


let suite = "sizeof tests" >:::
  ["sizeof primitives" >:: test_sizeof_primitives;
   "sizeof structs"    >:: test_sizeof_structs;
   "sizeof unions"     >:: test_sizeof_unions;
   "sizeof incomplete" >:: test_sizeof_incomplete;
   "sizeof void"       >:: test_sizeof_void;
   "sizeof arrays"     >:: test_sizeof_arrays;
   "sizeof pointers"   >:: test_sizeof_pointers;
   "sizeof views"      >:: test_sizeof_views;
  ]


let _ =
  run_test_tt_main suite
