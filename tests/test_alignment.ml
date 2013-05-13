open OUnit
open Ffi.C


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
let test_struct_tail_padding () = 
  let module M = struct
    open Type
    open Struct
    type a and b and u

    let struct_a = tag "A"
    let a = struct_a *:* char
    let b = struct_a *:* int
    let c = struct_a *:* char
    let () = seal (struct_a : a structure typ)

    let u = Union.tag "U"
    let x = Union.(u *:* char)
    let () = Union.seal (u : u union typ)

    let struct_b = tag "B"
    let d = struct_b *:* struct_a
    let e = struct_b *:* u
    let () = seal (struct_b : b structure typ)

    let char_ptr p = Ptr.(from_voidp char (to_voidp p))

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
        (Ptr.diff (char_ptr (pa |-> a)) (char_ptr (pa |-> b)))
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
        (Ptr.diff (char_ptr (pb |-> d)) (char_ptr (pb |-> e)))
        (3 * alignment int)
        ~printer:string_of_int;

      assert_equal
        ~msg:"sizeof(struct B) == 4 * alignmentof(int)"
        (sizeof struct_b) (4 * alignment int)
        ~printer:string_of_int;
    end
  end in ()


let suite = "Alignment tests" >:::
  ["struct tail padding" >:: test_struct_tail_padding;
  ]


let _ =
  run_test_tt_main suite
