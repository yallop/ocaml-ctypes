(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open OUnit
open Ctypes
open Unsigned


(* 
   Check that using a union to inspect the representation of a float (double)
   value gives the same result as Int64.of_bits.

     union u {
       double  f;
       int64_t i;
     };
*)
let test_inspecting_float () =
  let module M = struct
    type u
    let utyp : u union typ = union "u"
    let (-:) ty label = field utyp label ty
    let f = double  -: "f"
    let i = int64_t -: "i"
    let () = seal utyp

    let pi = 3.14
    let e = 2.718
    let u = make utyp
     
    (* Write through the double; read through the int64_t *)
    let () = setf u f pi
    let repr = getf u i
    let () = assert_equal (Int64.bits_of_float pi) repr

    (* Write through the int64_t; read through the double *)
    let () = setf u i (Int64.bits_of_float e)
    let e' = getf u f
    let () = assert_equal e e'

  end in ()


(* 
   Use a union with the following type to detect endianness

     union e {
       int64_t       i;
       unsigned char c[sizeof int64_t];
     };
*)
let test_endian_detection () =
  let module M = struct
    type e
    let etyp : e union typ = union "e"
    let (-:) ty label = field etyp label ty
    let i = int64_t                      -: "i"
    let c = array (sizeof int64_t) uchar -: "c"
    let () = seal etyp

    let updated_char_index =
      if Sys.big_endian then  sizeof int64_t - 1 else 0

    let e = make etyp

    let () = setf e i 1L

    let arr = getf e c

    module Array = CArray
    let () = assert_equal
      ~msg:"the byte that we expected to change was changed"
      arr.(updated_char_index) UChar.one

    let () = for i = 1 to sizeof int64_t - 1 do
        if i <> updated_char_index then
          assert_equal ~msg:"only the top or the bottom byte was changed"
            UChar.zero arr.(i)
      done
  end in ()


module Common_tests(S : Cstubs.FOREIGN with type 'a fn = 'a) =
struct
  open Functions
  module M = Stubs(S)
  open M
  (* Check that unions are tail-padded sufficiently to satisfy the alignment
     requirements of all their members.
  *)
  let test_union_padding () =
    let module M = struct
      let mkPadded : int64 -> padded union =
        fun x ->
          let u = make padded in
          setf u i x;
          u

      let arr = CArray.of_list padded [
        mkPadded 1L;
        mkPadded 2L;
        mkPadded 3L;
        mkPadded 4L;
        mkPadded 5L;
      ]

      let sum = sum_union_components
        (CArray.start arr)
        (Unsigned.Size_t.of_int (CArray.length arr))

      let () = assert_equal
        ~msg:"padded union members accessed correctly"
        15L sum
        ~printer:Int64.to_string
    end in ()
end

(* Check that the address of a union is equal to the addresses of each
   of its members.
*)
let test_union_address () =
  let module M = struct
    type u
    let u : u union typ = union "u"
    let (-:) ty label = field u label ty
    let i = int64_t                      -: "i"
    let c = char                         -: "c"
    let s = ptr (structure "incomplete") -: "s"
    let () = seal u

    let up = addr (make u)

    let () = begin

      assert_equal
        (to_voidp up) (to_voidp (up |-> i));

      assert_equal
        (to_voidp up) (to_voidp (up |-> c));

      assert_equal
        (to_voidp up) (to_voidp (up |-> s));
    end
  end in ()


(*
  Test that attempting to update a sealed union is treated as an error.
*)
let test_updating_sealed_union () =
  let utyp = union "sealed" in
  let _ = field utyp "_" int in
  let () = seal utyp in

  assert_raises (ModifyingSealedType "sealed")
    (fun () -> field utyp "_" char)


(*
  Test that attempting to seal an empty union is treated as an error.
*)
let test_sealing_empty_union () =
  let empty = union "empty" in

  assert_raises (Unsupported "union with no fields")
    (fun () -> seal empty)


module Foreign_tests = Common_tests(Tests_common.Foreign_binder)
module Stub_tests = Common_tests(Generated_bindings)


let suite = "Union tests" >:::
  ["inspecting float representation"
    >:: test_inspecting_float;

   "detecting endianness"
    >:: test_endian_detection;

   "union padding (foreign)"
    >:: Foreign_tests.test_union_padding;

   "union padding (stubs)"
    >:: Stub_tests.test_union_padding;

   "union address"
    >:: test_union_address;

   "updating sealed union"
    >:: test_updating_sealed_union;

   "sealing empty union"
    >:: test_sealing_empty_union;
  ]


let _ =
  run_test_tt_main suite
