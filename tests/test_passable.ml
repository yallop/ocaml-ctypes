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


(*
  Test struct passability.  Structs are passable unless they contain
  unpassable members (unions, arrays, or unpassable structs).
*)
let test_struct_passability () =
  let module M = struct
    open Struct
    open Type
    type s1 and s2 and s3 and s4 and s5 and u

    let s1 : s1 structure typ = tag "s1"
    let _ = s1 *:* int
    let _ = s1 *:* double
    let _ = s1 *:* ptr s1
    let _ = s1 *:* funptr (int @-> returning int)
    let () = seal s1

    let s2 : s2 structure typ = tag "s2"
    let _ = s2 *:* s1
    let _ = s2 *:* double
    let _ = s2 *:* ptr (array 10 int)
    let () = seal s2

    let s3 : s3 structure typ = tag "s3"
    let _ = s3 *:* array 10 (ptr char)
    let () = seal s3

    let s4 : s4 structure typ = tag "s4"
    let _ = s4 *:* s3
    let () = seal s4

    let u : u union typ = Union.tag "u"
    let _ = Union.(u *:* int)
    let () = Union.seal u

    let s5 : s5 structure typ = tag "s5"
    let _ = s5 *:* u
    let () = seal s5

    let _ = begin
      (* Struct types can be argument types *)
      ignore (s1 @-> returning void);
      ignore (s2 @-> returning void);

      (* Struct types can be return types *)
      ignore (void @-> returning s1);
      ignore (void @-> returning s2);

      assert_raises
        ~msg:"Structs with array members rejected as arguments"
        (Unsupported "Unsupported argument type")
        (fun () -> s3 @-> returning void);

      assert_raises
        ~msg:"Structs with array members rejected as return types"
        (Unsupported "Unsupported return type")
        (fun () -> void @-> returning s3);

      assert_raises
        ~msg:"Structs with unpassable struct members rejected as arguments"
        (Unsupported "Unsupported argument type")
        (fun () -> s4 @-> returning void);

      assert_raises
        ~msg:"Structs with unpassable struct members rejected as return types"
        (Unsupported "Unsupported return type")
        (fun () -> void @-> returning s4);

      assert_raises
        ~msg:"Structs with union members rejected as arguments"
        (Unsupported "Unsupported argument type")
        (fun () -> s5 @-> returning void);

      assert_raises
        ~msg:"Structs with union members rejected as return types"
        (Unsupported "Unsupported return type")
        (fun () -> void @-> returning s5);
    end
  end in ()


let suite = "Passability tests" >:::
  ["primitives are passable" >:: test_primitives_are_passable;
   "unions are not passable" >:: test_unions_are_not_passable;
   "struct passability" >:: test_struct_passability;
  ]


let _ =
  run_test_tt_main suite
