open OUnit
open Ffi.C


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
    open Union
    open Type
    type u
    let utyp : u union typ = tag "u"
    let f = utyp *:* double
    let i = utyp *:* int64_t
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
    open Union
    open Type
    type e
    let etyp : e union typ = tag "e"
    let i = etyp *:* int64_t
    let c = etyp *:* array (sizeof int64_t) uchar
    let () = seal etyp

    let updated_char_index =
      if Sys.big_endian then  sizeof int64_t - 1 else 0

    let e = make etyp

    let () = setf e i 1L

    let arr = getf e c

    let () = assert_equal
      ~msg:"the byte that we expected to change was changed"
      arr.(updated_char_index) 1

    let () = for i = 1 to sizeof int64_t - 1 do
        if i <> updated_char_index then
          assert_equal ~msg:"only the top or the bottom byte was changed"
            0 arr.(i)
      done
  end in ()



let suite = "Union tests" >:::
  ["inspecting float representation" >:: test_inspecting_float;
   "detecting endianness" >:: test_endian_detection;
  ]


let _ =
  run_test_tt_main suite
