open OUnit
open Ffi.C


(*
  Test some relationships between the sizes of primitive types.
*)
let test_sizeof_primitives () = Type.(
  assert_equal ~msg:"sizeof char == 1"
    (sizeof char) 1;

  assert_bool "sizeof char <= sizeof int"
    (sizeof char <= sizeof int);

  assert_bool "sizeof float <= sizeof double"
    (sizeof char <= sizeof int);

  assert_bool "sizeof short <= sizeof int"
    (sizeof char <= sizeof int);

  assert_bool "sizeof int <= sizeof long"
    (sizeof char <= sizeof int);

  assert_equal ~msg:"2 * sizeof int32_t == sizeof int64_t"
    (2 * sizeof int32_t) (sizeof int64_t);

  assert_equal ~msg:"2 * sizeof int16_t == sizeof int32_t"
    (2 * sizeof int16_t) (sizeof int32_t);

  assert_equal ~msg:"2 * sizeof int8_t == sizeof int16_t"
    (2 * sizeof int8_t) (sizeof int16_t);

  assert_bool "sizeof int16_t <= sizeof int"
    (sizeof int16_t <= sizeof int);

  assert_bool "sizeof int32_t <= sizeof long"
    (sizeof int16_t <= sizeof int);
)



(*
  Test some properties of the sizes of structs.
*)
let test_sizeof_unions () = Type.(
  let open Union in

  let int_char = tag "int_char" in
  let _ = int_char *:* int in
  let _ = int_char *:* char in
  let _ = seal int_char in
  
  assert_equal (sizeof int) (sizeof int_char);


  let char17 = tag "char17" in
  let _ = char17 *:* array 17 char in
  let _ = seal char17 in
  
  assert_equal 17 (sizeof char17)
)



(*
  Test some properties of the sizes of structs.
*)
let test_sizeof_structs () = Type.(
  let module M = struct
    open Struct

    (* We don't expect homogeneous structs consisting of words to have
       any padding. *)
    type h
    let () =
      for i = 1 to 10 do
        let homogeneous : h structure typ = tag "h" in
        for j = 1 to i do
          ignore (homogeneous *:* int);
        done;
        seal homogeneous;
        assert_equal (i * sizeof int) (sizeof homogeneous)
      done

  end in ())
  

(*
  Test that taking the size of void is treated as an error.
*)
let test_sizeof_void () = Type.(
  assert_raises IncompleteType
    (fun () -> sizeof void)
)
 

(*
  Test the behaviour of sizeof on array types.
*)
let test_sizeof_arrays () = Type.(
  assert_equal ~msg:"The size of an array is the sum of the size of its members"
    (12 * (sizeof int8_t)) (sizeof (array 12 int8_t));

  assert_equal ~msg:"Arrays of arrays are correctly sized"
    (5 * 7 * (sizeof nativeint)) (sizeof (array 7 (array 5 nativeint)));
)
 

(*
  Test that all pointers have equal size.
*)
let test_sizeof_pointers () = Type.(
  let pointer_size = sizeof (ptr void) in
  assert_equal pointer_size (sizeof (ptr void));
  assert_equal pointer_size (sizeof (ptr int));
  assert_equal pointer_size (sizeof (funptr (int @-> returning int)));
  assert_equal pointer_size (sizeof (ptr (ptr void)));
  let module M = struct
    open Struct
    type t
    let t : t structure typ = tag "t"
    let c = t *:* int
    let f = t *:* double
    let () = seal t
  end in
  assert_equal pointer_size (sizeof (ptr M.t))
)


let suite = "sizeof tests" >:::
  ["sizeof primitives" >:: test_sizeof_primitives;
   "sizeof structs"    >:: test_sizeof_structs;
   "sizeof unions"     >:: test_sizeof_unions;
   "sizeof void"       >:: test_sizeof_void;
   "sizeof arrays"     >:: test_sizeof_arrays;
   "sizeof pointers"   >:: test_sizeof_pointers;
  ]


let _ =
  run_test_tt_main suite
