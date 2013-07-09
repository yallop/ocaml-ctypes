(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open OUnit
open Ctypes


(*
  Test that primitives are passable.
*)
let test_primitives_are_passable () =
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
  in ()


(*
  Test that unions are not passable
*)
let test_unions_are_not_passable () =
  let module M = struct
    type u

    let u : u union typ = union "u"
    let c = u +:+ int
    let f = u +:+ double
    let p = u +:+ ptr u
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
  Test that complex values are not passable
*)
let test_complex_values_are_not_passable () =
  assert_raises ~msg:"complex32 type rejected as argument"
    (Unsupported "Unsupported argument type")
    (fun () -> complex32 @-> returning void);

  assert_raises ~msg:"complex64 type rejected as argument"
    (Unsupported "Unsupported argument type")
    (fun () -> complex64 @-> returning void);
  
  assert_raises ~msg:"complex32 type rejected as return type"
    (Unsupported "Unsupported return type")
    (fun () -> void @-> returning complex32);

  assert_raises ~msg:"complex64 type rejected as return type"
    (Unsupported "Unsupported return type")
    (fun () -> void @-> returning complex64)


(*
  Test that arrays are not passable
*)
let test_arrays_are_not_passable () =
  assert_raises ~msg:"Array type rejected as argument"
    (Unsupported "Unsupported argument type")
    (fun () -> array 1 int @-> returning void);
      
  assert_raises ~msg:"Array type rejected as return type"
    (Unsupported "Unsupported return type")
    (fun () -> void @-> returning (array 1 int))


(*
  Test that pointers are passable
*)
let test_pointers_are_passable () =
  (* Pointers to primitives are passable *)
  let _ = ptr void @-> returning (ptr void)
  and _ = ptr int @-> returning (ptr int)
  and _ = ptr (ptr int) @-> returning (ptr (ptr int))
  in

  (* Pointers to unpassable types are passable *)
  let module M = struct
    type s1 and u

    let s1 : s1 structure typ = structure "s1"
    let _ = s1 *:* int
    let _ = s1 *:* ptr s1
    let () = seal s1

    let u : u union typ = union "u"
    let _ = u +:+ int
    let () = seal u
  end in
  let open M in

  let _ = ptr s1 @-> returning (ptr s1)
  and _ = ptr u @-> returning (ptr u) in
  ()


(*
  Test that function pointers are passable
*)
let test_function_pointers_are_passable () =
  (* Pointers to primitives are passable *)
  ignore (funptr (int @-> returning int)
          @-> returning (funptr (int @-> returning int)))


(*
  Test that values of abstract types are not passable
*)
let test_abstract_values_are_not_passable () = begin
  assert_raises ~msg:"Abstract type rejected as argument"
    (Unsupported "Unsupported argument type")
    (fun () ->
      (abstract ~name:"abstract" ~size:1 ~alignment:1) @-> returning void);

  assert_raises ~msg:"Abstract type rejected as return type"
    (Unsupported "Unsupported return type")
    (fun () ->
      void @-> returning (abstract ~name:"abstract" ~size:1 ~alignment:1));
end


(*
  Test struct passability.  Structs are passable unless they contain
  unpassable members (unions, arrays, abstract types, or unpassable structs).
*)
let test_struct_passability () =
  let module M = struct
    type s1 and s2 and s3 and s4 and s5 and s6 and u

    let s1 : s1 structure typ = structure "s1"
    let _ = s1 *:* int
    let _ = s1 *:* double
    let _ = s1 *:* ptr s1
    let _ = s1 *:* funptr (int @-> returning int)
    let () = seal s1

    let s2 : s2 structure typ = structure "s2"
    let _ = s2 *:* s1
    let _ = s2 *:* double
    let _ = s2 *:* ptr (array 10 int)
    let () = seal s2

    let s3 : s3 structure typ = structure "s3"
    let _ = s3 *:* array 10 (ptr char)
    let () = seal s3

    let s4 : s4 structure typ = structure "s4"
    let _ = s4 *:* s3
    let () = seal s4

    let u : u union typ = union "u"
    let _ = u +:+ int
    let () = seal u

    let s5 : s5 structure typ = structure "s5"
    let _ = s5 *:* u
    let () = seal s5

    let s6 : s6 structure typ = structure "s6"
    let _ = s6 *:* abstract ~name:"abstract" ~size:1 ~alignment:1
    let () = seal s6

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

      assert_raises
        ~msg:"Structs with abstract members rejected as arguments"
        (Unsupported "Unsupported argument type")
        (fun () -> s6 @-> returning void);

      assert_raises
        ~msg:"Structs with abstract members rejected as return types"
        (Unsupported "Unsupported return type")
        (fun () -> void @-> returning s6);
    end
  end in ()


(*
  Test passability of incomplete types.  Trying to use an incomplete type
  in a function specification should give rise to an error.
*)
let test_incomplete_passability () =
  let s = structure "incomplete"
  and u = union "incomplete"
  in begin
    assert_raises IncompleteType
      (fun () -> s @-> returning void);
    
    assert_raises IncompleteType
      (fun () -> void @-> returning s);
    
    assert_raises IncompleteType
      (fun () -> u @-> returning void);
    
    assert_raises IncompleteType
      (fun () -> void @-> returning u);
  end


let suite = "Passability tests" >:::
  ["primitives are passable"
    >:: test_primitives_are_passable;

   "unions are not passable"
    >:: test_unions_are_not_passable;

   "complex values are not passable"
    >:: test_complex_values_are_not_passable;

   "arrays are not passable"
    >:: test_arrays_are_not_passable;

   "pointers are passable"
    >:: test_pointers_are_passable;

   "function pointers are passable"
    >:: test_function_pointers_are_passable;

   "abstract values are not passable"
    >:: test_abstract_values_are_not_passable;

   "struct passability"
    >:: test_struct_passability;

   "incomplete types are not passable"
    >:: test_incomplete_passability;
  ]


let _ =
  run_test_tt_main suite
