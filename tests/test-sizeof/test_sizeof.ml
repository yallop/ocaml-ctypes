(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open OUnit2
open Ctypes


(*
  Test some relationships between the sizes of primitive types.
*)
let test_sizeof_primitives _ = begin
  assert_equal ~msg:"sizeof (char) == 1"
    (sizeof char) 1;

  assert_equal ~msg:"sizeof (signed char) == 1"
    (sizeof schar) 1;

  assert_bool "sizeof (char) <= sizeof (int)"
    (sizeof char <= sizeof int);

  assert_bool "sizeof (float) <= sizeof (double)"
    (sizeof char <= sizeof int);

  assert_bool "sizeof (short) <= sizeof (int)"
    (sizeof char <= sizeof int);

  assert_equal ~msg:"2 * sizeof (int32_t) == sizeof (int64_t)"
    (2 * sizeof int32_t) (sizeof int64_t);

  assert_equal ~msg:"2 * sizeof (int16_t) == sizeof (int32_t)"
    (2 * sizeof int16_t) (sizeof int32_t);

  assert_equal ~msg:"2 * sizeof (int8_t) == sizeof (int16_t)"
    (2 * sizeof int8_t) (sizeof int16_t);

  assert_bool "sizeof (int16_t) <= sizeof (int)"
    (sizeof int16_t <= sizeof int);
end



(*
  Test some properties of the sizes of structs.
*)
let test_sizeof_structs _ =
  let module M = struct
    (* We don't expect homogeneous structs consisting of words to have
       any padding. *)
    type h
    let () =
      for i = 1 to 10 do
        let homogeneous : h structure typ = structure "h" in
        for j = 1 to i do
          ignore (field homogeneous "_" int);
        done;
        seal homogeneous;
        assert_equal (i * sizeof int) (sizeof homogeneous)
      done

  end in ()


(*
  Test that taking the size of an incomplete type is treated as an error.
*)
let test_sizeof_incomplete _ = begin
  assert_raises IncompleteType
    (fun () -> sizeof (structure "incomplete"));
end
  

(*
  Test that taking the size of void is treated as an error.
*)
let test_sizeof_void _ =
  assert_raises IncompleteType
    (fun () -> sizeof void)
 

(*
  Test that all pointers have equal size.
*)
let test_sizeof_pointers _ = begin
  let pointer_size = sizeof (ptr void) in
  assert_equal pointer_size (sizeof (ptr void));
  assert_equal pointer_size (sizeof (ptr int));
  assert_equal pointer_size (sizeof (Foreign.funptr (int @-> returning int)));
  assert_equal pointer_size (sizeof (ptr (ptr void)));
  let module M = struct
    type t
    let t : t structure typ = structure "t"
    let c = field t "c" int
    let f = field t "f" double
    let () = seal t
  end in
  assert_equal pointer_size (sizeof (ptr M.t))
end


(*
  Test that the size of a view type is the same as the underlying type.
*)
let test_sizeof_views _ = begin
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
  ["sizeof primitives"
   >:: test_sizeof_primitives;
   
   "sizeof structs"
   >:: test_sizeof_structs;
   
   "sizeof incomplete"
   >:: test_sizeof_incomplete;

   "sizeof void"
   >:: test_sizeof_void;

   "sizeof pointers"
   >:: test_sizeof_pointers;

   "sizeof views"
   >:: test_sizeof_views;
  ]


let _ =
  run_test_tt_main suite
