(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open OUnit2
open Ctypes


(*
  Test some relationships between the alignment requirements of primitive types.
*)
let test_primitive_alignment _ = begin
  assert_equal ~msg:"alignmentof(char) == 1"
    (alignment char) 1;

  assert_equal ~msg:"alignmentof(signed char) == 1"
    (alignment schar) 1;

  assert_equal ~msg:"alignmentof(complex32) == alignmentof(float)"
    (alignment complex32) (alignment float);

  assert_equal ~msg:"alignmentof(complex64) == alignmentof(double)"
    (alignment complex64) (alignment double);
end


(*
  Test that requesting the alignment of an incomplete type raises an exception.
*)
let test_incomplete_alignment _ =
  assert_raises IncompleteType
    (fun () -> alignment void);

  let module M = struct
    let t = structure "t"
    let i = field t "i" int

    let () =
      assert_raises IncompleteType
        (fun () -> alignment t)
  end in

  let module M = struct
    let u = union "u"
    let i = field u "i" int

    let () =
      assert_raises IncompleteType
        (fun () -> alignment u)
  end in
  ()


(* 
   Test that the alignment of a struct is equal to the maximum
   alignment of its members.
*)
let test_struct_alignment _ = 
  let module M = struct
    type a and b and u

    let maximum = List.fold_left max 0

    let struct_a = structure "A"
    let (-:) ty label = field struct_a label ty
    let _ = char   -: "_"
    let _ = int    -: "_"
    let _ = double -: "_"
    let () = seal struct_a

    let () = assert_equal
      (maximum [alignment char;
                alignment int;
                alignment double])
      (alignment struct_a)

    let charish = view ~read:(fun _ -> ()) ~write:(fun () -> 'c') char

    let struct_b = structure "A"
    let (-:) ty label = field struct_b label ty
    let _ = charish                        -: "_"
    let _ = Foreign.funptr (int @-> returning int) -: "_"
    let _ = double                         -: "_"
    let () = seal struct_b

    let () = assert_equal
      (maximum [alignment charish;
                alignment (Foreign.funptr (int @-> returning int));
                alignment double])
      (alignment struct_b)
  end in ()


(*
  Test that structs are properly tail-padded.  For example, suppose a 32-bit
  architecture with 8-bit bytes and word-aligned ints and the following
  definitions:

     struct A { char a; int b; char c; };
     struct B { struct A d; char e; }

  Then we should have the following layouts:

     A: a---bbbbc---
     B: A-----------e---

  and the following sizes:

     sizeof (struct A) == 12
     sizeof (struct B) == 16
*)
let test_struct_tail_padding _ = 
  let module M = struct
    type a and b and u

    let struct_a = structure "A"
    let (-:) ty label = field struct_a label ty
    let a = char -: "a"
    let b = int  -: "b"
    let c = char -: "c"
    let () = seal (struct_a : a structure typ)

    let u = union "U"
    let (-:) ty label = field u label ty
    let x = char -: "x"
    let () = seal (u : u union typ)

    let struct_b = structure "B"
    let (-:) ty label = field struct_b label ty
    let d = struct_a -: "d"
    let e = u        -: "e"
    let () = seal (struct_b : b structure typ)

    let char_ptr p = from_voidp char (to_voidp p)

    let va = make struct_a and vb = make struct_b
    let pa = addr va and pb = addr vb

    let () = begin
      assert_equal
        ~msg:"offsetof (A, a) == 0"
        (offsetof a) 0
        ~printer:string_of_int;

      assert_equal
        ~msg:"offsetof(A, b) == alignmentof(int)"
        (offsetof b) (alignment int)
        ~printer:string_of_int;

      assert_equal
        ~msg:"((char *)&pa->b - (char *)&pa->a) == alignmentof(int)"
        (ptr_diff (char_ptr (pa |-> a)) (char_ptr (pa |-> b)))
        (alignment int)
        ~printer:string_of_int;

      assert_equal
        ~msg:"offsetof(A, c) == 2 * alignmentof(int)"
        (offsetof c) (2 * alignment int)
        ~printer:string_of_int;

      assert_equal
        ~msg:"sizeof(struct A) == 3 * alignmentof(int)"
        (sizeof struct_a)  (3 * alignment int)
        ~printer:string_of_int;

      assert_equal
        ~msg:"offsetof(B, e) == 3 * alignmentof(int)"
        (offsetof e) (3 * alignment int)
        ~printer:string_of_int;

      assert_equal
        ~msg:"((char *)&pb->e - (char *)&pb->d) == 3 * alignmentof(int)"
        (ptr_diff (char_ptr (pb |-> d)) (char_ptr (pb |-> e)))
        (3 * alignment int)
        ~printer:string_of_int;

      assert_equal
        ~msg:"sizeof(struct B) == 4 * alignmentof(int)"
        (sizeof struct_b) (4 * alignment int)
        ~printer:string_of_int;
    end
  end in ()


let suite = "Alignment tests" >:::
  ["struct tail padding"
    >:: test_struct_tail_padding;

   "primitive alignment"
   >:: test_primitive_alignment;

   "struct alignment"
   >:: test_struct_alignment;

   "alignment of incomplete types"
   >:: test_incomplete_alignment;
  ]


let _ =
  run_test_tt_main suite
