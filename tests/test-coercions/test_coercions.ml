(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open OUnit2
open Ctypes


(* 
   Check coercions between pointers.
*)
let test_pointer_coercions _ =
  let module M = struct
    type boxed_type = T : 'a typ -> boxed_type
    let types = [
      T void;
      T int8_t;
      T int;
      T float;
      T short;
      T (ptr double);
      T string;
      T (array 5 int32_t);
      T (structure "s");
    ]

    (* Check that we can construct a coercion between any two pointer types *)
    let () =
      ListLabels.iter types
        ~f:(fun (T t1) ->
          ListLabels.iter types
            ~f:(fun (T t2) ->
              let _ = coerce (ptr t1) (ptr t2) in ()))

    (* Check that pointer coercions are value-preserving. *)
    let v = 10
    let p = allocate int v
    let p' = coerce (ptr float) (ptr int) (coerce (ptr int) (ptr float) p)

    let () = assert_equal p p'
  end in ()


(*
  Check that coercions between a pointer to a struct and a pointer to
  its first member succeed.
*)
let test_struct_first_member_coercions _ =
  let module M = struct
    let s = structure "s"
    let f = field s "f" double
    let i = field s "i" int
    let () = seal s

    let () = begin
      let v = make s in
      let p = coerce (ptr s) (ptr double) (addr v) in

      setf v f 5.5;
      assert_equal !@p 5.5;

      p <-@ 6.6;
      assert_equal (getf v f) 6.6
    end
  end in ()


(* 
   Check coercions between views.
*)
let test_view_coercions _ =
  let module M = struct
    type 'a variant = V of 'a
    let unV (V v) = v and inV v = V v
    let variant_view v = view v ~read:inV ~write:unV

    type 'a record = { r : 'a }
    let record_view v = view v
      ~read:(fun r -> {r})
      ~write:(fun {r} -> r)
      
    let pintvv = variant_view (variant_view (ptr int))
    let pintr = record_view (ptr int)
      
    let () = begin
      let pi = allocate int 100 in
      let v = allocate pintvv (V (V pi)) in

      assert_equal
        !@((coerce pintvv pintr !@v).r)
        100
    end

  end in ()


module Common_tests(S : Tests_common.FOREIGN with type 'a fn = 'a) =
struct
  module M = Functions.Stubs(S)
  open M

  (* 
     Check coercions between functions.
  *)
  let test_function_coercions _ =
    let i64int = view int
      ~read:Int64.of_int ~write:Int64.to_int in
    let memchr' = coerce_fn
      (ptr void @-> int @-> int @-> returning (ptr void))
      (string @-> int8_t @-> i64int @-> returning string_opt)
      memchr in
    begin
      assert_equal
        (memchr' "foobar" (Char.code 'b') 4L)
        (Some "bar")
      ;
      assert_equal
        (memchr' "foobar" (Char.code 'b') 2L)
        None
      ;
    end
end

(* 
   Check that identity coercions are cost-free.
*)
let test_identity_coercions _ =
  let f = fun x y -> x in
  let fn = int @-> float @-> returning int in
  let f' = coerce_fn fn fn f in
  assert_bool "identity coercions are free" (f' == f)


(* 
   Check that coercions between unsupported types raise an exception
*)
let test_unsupported_coercions _ =
  let module M = struct
    type boxed_type = T : 'a typ -> boxed_type
    let types = [
      T int8_t,
      [T float;
       T (array 5 int32_t); T (structure "s")];

      T int,
      [T float;
       T (array 5 int32_t); T (structure "s")];

      T float,
      [T int8_t; T int; T short;
       T (array 5 int32_t); T (structure "s")];

      T short,
      [T float;
       T (array 5 int32_t); T (structure "s")];

      T (array 5 int32_t),
      [T int8_t; T int; T float; T short;
       T (array 5 int32_t); T (structure "s")];

      T (structure "s"),
      [T int8_t; T int; T float; T short;
       T (array 5 int32_t); T (structure "s")];
    ]

    (* None of the types in the list are currently intercoercible. *)
    let () = 
      ListLabels.iter types ~f:(fun (T t1, ts) ->
      ListLabels.iter ts ~f:(fun (T t2) ->
        assert_raises Uncoercible (fun () -> coerce t1 t2)))
  end in ()


module Foreign_tests = Common_tests(Tests_common.Foreign_binder)


let suite = "Coercsion tests" >:::
  ["test pointer coercions"
    >:: test_pointer_coercions;

   "test struct first member coercions"
    >:: test_struct_first_member_coercions;

   "test view coercions"
    >:: test_view_coercions;

   "test function coercions (foreign)"
    >:: Foreign_tests.test_function_coercions;

   "test identity coercions"
    >:: test_identity_coercions;

   "test unsupported coercions"
    >:: test_unsupported_coercions;
  ]


let _ =
  run_test_tt_main suite
