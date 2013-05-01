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
     
    let u = make utyp

    let () = setf u f pi
    let repr = getf u i

    let () = assert_equal (Int64.bits_of_float pi) repr
  end in ()


let suite = "Union tests" >:::
  ["inspecting float representation" >:: test_inspecting_float;
  ]


let _ =
  run_test_tt_main suite
