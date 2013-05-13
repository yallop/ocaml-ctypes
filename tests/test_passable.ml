open OUnit
open Ffi.C


(*
  Test that primitives are passable.
*)
let test_primitives_are_passable () =
  let open Type in
  let _ = void @-> returning void 
  and _ = char @-> returning char
  and _ = schar @-> returning schar
  and _ = float @-> returning float
  and _ = double @-> returning double
  and _ = int   @-> returning int  
  and _ = nativeint @-> returning nativeint
  and _ = int8_t @-> returning int8_t
  and _ = short @-> returning short
  and _ = int16_t @-> returning int16_t
  and _ = int32_t @-> returning int32_t
  and _ = int64_t @-> returning int64_t
  and _ = uchar @-> returning uchar
  and _ = uint8_t @-> returning uint8_t
  and _ = uint16_t @-> returning uint16_t
  and _ = uint32_t @-> returning uint32_t
  and _ = uint64_t @-> returning uint64_t
  and _ = size_t @-> returning size_t
  and _ = ushort @-> returning ushort
  and _ = uint @-> returning uint
  and _ = ulong @-> returning ulong
  and _ = ullong @-> returning ullong
  and _ = string @-> returning string
  in ()


(*
  Test that unions are not passable
*)
let test_unions_are_not_passable () =
  let module M = struct
    open Union
    open Type
    type u

    let u : u union typ = tag "u"
    let c = u *:* int
    let f = u *:* double
    let p = u *:* ptr u
    let () = seal u

    let _ = begin
      assert_raises ~msg:"Union type rejected as argument"
        (Unsupported "Unsupported argument type")
        (fun () -> u @-> returning void);
      
      assert_raises ~msg:"Union type rejected as return type"
        (Unsupported "Unsupported return type")
        (fun () -> void @-> returning u);
    end
  end in ()


let suite = "Passability tests" >:::
  ["primitives are passable" >:: test_primitives_are_passable;
   "unions are not passable" >:: test_unions_are_not_passable;
  ]


let _ =
  run_test_tt_main suite
