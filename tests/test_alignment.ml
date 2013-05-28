open OUnit
open Ctypes


(*
  Test some relationships between the alignment requirements of primitive types.
*)
let test_primitive_alignment () = begin
  assert_equal ~msg:"alignmentof(char) == 1"
    (alignment char) 1;

  assert_equal ~msg:"alignmentof(signed char) == 1"
    (alignment schar) 1;

  assert_equal ~msg:"alignmentof(unsigned char) == 1"
    (alignment uchar) 1;

  assert_equal ~msg:"alignmentof(short) == alignmentof(unsigned short)"
    (alignment short) (alignment ushort);

  assert_equal ~msg:"alignmentof(int) == alignmentof(unsigned int)"
    (alignment int) (alignment uint);

  assert_equal ~msg:"alignmentof(long) == alignmentof(unsigned long)"
    (alignment long) (alignment ulong);

  assert_equal ~msg:"alignmentof(long long) == alignmentof(unsigned long long)"
    (alignment llong) (alignment ullong);

  assert_equal ~msg:"alignmentof(int8_t) == alignmentof(uint8_t)"
    (alignment int8_t) (alignment uint8_t);

  assert_equal ~msg:"alignmentof(int16_t) == alignmentof(uint16_t)"
    (alignment int16_t) (alignment uint16_t);

  assert_equal ~msg:"alignmentof(int32_t) == alignmentof(uint32_t)"
    (alignment int32_t) (alignment uint32_t);

  assert_equal ~msg:"alignmentof(int64_t) == alignmentof(uint64_t)"
    (alignment int64_t) (alignment uint64_t);
end


(*
  Test the alignment of abstract types
*)
let test_abstract_alignment () =
  for i = 1 to 10 do
    assert_equal
      i (alignment (abstract ~size:(11 - i) ~alignment:i))
  done


(*
  Test that requesting the alignment of an incomplete type raises an exception.
*)
let test_incomplete_alignment () =
  assert_raises IncompleteType
    (fun () -> alignment void);

  let module M = struct
    open Struct
    let t = structure "t"
    let i = t *:* int
      
    let () =
      assert_raises IncompleteType
        (fun () -> alignment t)
  end in

  let module M = struct
    open Union
    let u = union "u"
    let i = u +:+ int
      
    let () =
      assert_raises IncompleteType
        (fun () -> alignment u)
  end in
  ()


(* 
   Test that the alignment of a struct is equal to the maximum
   alignment of its members.
*)
let test_struct_alignment () = 
  let module M = struct
    open Struct
    type a and b and u

    let maximum = List.fold_left max 0

    let struct_a = structure "A"
    let _ = struct_a *:* char
    let _ = struct_a *:* int
    let _ = struct_a *:* double
    let () = seals struct_a

    let () = assert_equal
      (maximum [alignment char;
                alignment int;
                alignment double])
      (alignment struct_a)

    let abs = abstract ~size:33 ~alignment:33
    let charish = view ~read:(fun _ -> ()) ~write:(fun () -> 'c') char

    let struct_b = structure "A"
    let _ = struct_b *:* charish
    let _ = struct_b *:* funptr (int @-> returning int)
    let _ = struct_b *:* abs
    let _ = struct_b *:* double
    let () = seals struct_b

    let () = assert_equal
      (maximum [alignment charish;
                alignment (funptr (int @-> returning int));
                alignment abs;
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
let test_struct_tail_padding () = 
  let module M = struct
    open Union
    open Struct
    type a and b and u

    let struct_a = structure "A"
    let a = struct_a *:* char
    let b = struct_a *:* int
    let c = struct_a *:* char
    let () = seals (struct_a : a structure typ)

    let u = union "U"
    let x = u +:+ char
    let () = sealu (u : u union typ)

    let struct_b = structure "B"
    let d = struct_b *:* struct_a
    let e = struct_b *:* u
    let () = seals (struct_b : b structure typ)

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
   "primitive alignment" >:: test_primitive_alignment;
   "struct alignment" >:: test_struct_alignment;
   "alignment of abstract types" >:: test_abstract_alignment;
   "alignment of incomplete types" >:: test_incomplete_alignment;
  ]


let _ =
  run_test_tt_main suite
