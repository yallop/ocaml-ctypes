(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open OUnit
open Ctypes


let strip_whitespace = Str.(global_replace (regexp "[\n ]+") "")

let equal_ignoring_whitespace l r =
  strip_whitespace l = strip_whitespace r

let assert_printed_as ?name format expected typ =
 assert_equal
   ~cmp:equal_ignoring_whitespace 
   ~printer:(fun s -> s)
   expected (format ?name typ)

let assert_typ_printed_as ?name e t = assert_printed_as ?name string_of_typ e t
let assert_fn_printed_as ?name e f = assert_printed_as ?name string_of_fn e f


(*
  Test the printing of atomic types: void, arithmetic types and abstract
  types.
*)
let test_atomic_printing () =
  begin
    assert_typ_printed_as "void"
      void;

    assert_typ_printed_as ~name:"a" "char a"
      char;

    assert_typ_printed_as "signed char"
      schar;

    assert_typ_printed_as ~name:"b" "short b"
      short;

    assert_typ_printed_as "int"
      int;

    assert_typ_printed_as ~name:"c" "long c"
      long;

    assert_typ_printed_as "long long"
      llong;

    assert_typ_printed_as ~name:"d" "int d"
      nativeint;

    assert_typ_printed_as "int8_t"
      int8_t;

    assert_typ_printed_as ~name:"e" "int16_t e"
      int16_t;

    assert_typ_printed_as "int32_t"
      int32_t;

    assert_typ_printed_as ~name:"f" "int64_t f"
      int64_t;

    assert_typ_printed_as "unsigned char"
      uchar;

    assert_typ_printed_as ~name:"g" "uint8_t g"
      uint8_t;

    assert_typ_printed_as "uint16_t"
      uint16_t;

    assert_typ_printed_as ~name:"h" "uint32_t h"
      uint32_t;

    assert_typ_printed_as "uint64_t"
      uint64_t;

    assert_typ_printed_as ~name:"i" "size_t i"
      size_t;

    assert_typ_printed_as "unsigned short"
      ushort;

    assert_typ_printed_as ~name:"j" "unsigned int j"
      uint;

    assert_typ_printed_as "unsigned long"
      ulong;

    assert_typ_printed_as ~name:"k" "unsigned long long k"
      ullong;

    assert_typ_printed_as "float"
      float;

    assert_typ_printed_as ~name:"l" "double l"
      double;

    let abs_t = abstract ~name:"abs_t" ~size:1 ~alignment:1 in

    assert_typ_printed_as "abs_t"
      abs_t;
  end


(*
  Test the printing of pointers to object and function types.
*)
let test_pointer_printing () =
  begin
    (* Pointers to atomic types *)
    assert_typ_printed_as ~name:"a" "void *a"
      (ptr void);

    assert_typ_printed_as "unsigned long long **"
      (ptr (ptr ullong));

    assert_typ_printed_as ~name:"b" "char *****b"
      (ptr (ptr (ptr (ptr (ptr char)))));

    let abs_t = abstract ~name:"abs_t" ~size:1 ~alignment:1 in

    assert_typ_printed_as "abs_t *"
      (ptr abs_t);

    (* Pointers to incomplete structs and unions *)
    let s_incomplete = structure "s_incomplete" in 
    let u_incomplete = union "u_incomplete" in 

    assert_typ_printed_as ~name:"c" "struct s_incomplete *c"
      (ptr s_incomplete);

    assert_typ_printed_as "union u_incomplete **"
      (ptr (ptr u_incomplete));

    (* Pointers to complete structs and unions *)
    let s_complete = structure "s_complete" in
    let _ = s_complete *:* int in
    seal s_complete;

    let u_complete = union "u_complete" in 
    let _ = u_complete +:+ int in
    seal u_complete;

    assert_typ_printed_as ~name:"d" "struct s_complete *d"
      (ptr s_complete);

    assert_typ_printed_as "union u_complete **"
      (ptr (ptr u_complete));

    (* Pointers to arrays *)
    assert_typ_printed_as ~name:"e" "int (*e)[4]"
      (ptr (array 4 int));

    assert_typ_printed_as "struct s_complete (*)[3]"
      (ptr (array 3 s_complete));

    assert_typ_printed_as ~name:"f" "union u_complete (*f)[3][4][5]"
      (ptr (array 3 (array 4 (array 5 u_complete))));

    (* Pointers to functions *)
    assert_typ_printed_as "void (*)(void)"
      (funptr (void @-> returning void));

    assert_typ_printed_as ~name:"g" "float (*g)(int, long)"
      (funptr (int @-> long @-> returning float));

    assert_typ_printed_as "void (*)(int (*)[4])"
      (funptr (ptr (array 4 int) @-> returning void));

    assert_typ_printed_as ~name:"h" "int32_t (*(*h)(void ))(int)"
      (funptr (void @-> returning (funptr (int @-> returning int32_t))));

    assert_typ_printed_as
      "unsigned long (*(*)(int, void (*)(float, float)))(long)"
      (funptr (int @->
               funptr (float @-> float @-> returning void) @->
               returning (funptr (long @-> returning ulong))));

    (* Pointers to pointers to functions *)
    assert_typ_printed_as ~name:"i" "double (**i)(int)"
      (ptr (funptr (int @-> returning double)));

    assert_typ_printed_as "double (**)(int)"
      (ptr (funptr (int @-> returning double)));

    assert_typ_printed_as ~name:"j" "void (*(*(*(**j)(int))(void))[8])(long, long)"
      (ptr
         (funptr
            (int @->
             returning (funptr
                          (void @->
                           returning (ptr
                                        (array 8
                                           (funptr
                                              (long @->
                                               long @->
                                               returning void)))))))));
  end


(*
  Test the printing of pointers to object and function types.
*)
let test_struct_and_union_printing () =
  begin
    (* Incomplete structs and unions *) 
    let s_incomplete = structure "s_incomplete" in 
    let u_incomplete = union "u_incomplete" in 
    
    assert_typ_printed_as ~name:"a" "struct s_incomplete a"
      s_incomplete;
    
    assert_typ_printed_as "union u_incomplete"
      u_incomplete;

    (* Structs and unions containing primitives *)
    let s_prims = structure "s_prims" in
    let _ = s_prims *:* int in
    let _ = s_prims *:* ulong in
    let _ = s_prims *:* float in
    seal s_prims;

    assert_typ_printed_as ~name:"b"
                          "struct s_prims {
                              int field_0;
                              unsigned long field_1;
                              float field_2;
                           } b"
      s_prims;

    let u_prims = union "u_prims" in
    let _ = u_prims +:+ int32_t in
    let _ = u_prims +:+ int64_t in
    let _ = u_prims +:+ double in
    seal u_prims;

    assert_typ_printed_as "union u_prims {
                              int32_t field_0;
                              int64_t field_1;
                              double field_2;
                           }"
      u_prims;

    (* Structs and unions containing pointers to themselves *)
    let selfish = structure "selfish" in
    let _ = selfish *:* ptr selfish in
    let _ = selfish *:* ptr int in
    let _ = selfish *:* ptr (ptr selfish) in
    seal selfish;

    assert_typ_printed_as ~name:"c"
                          "struct selfish {
                              struct selfish *field_0;
                              int *field_1;
                              struct selfish **field_2;
                           } c"
      selfish;

    let u_selfish = union "u_selfish" in
    let _ = u_selfish +:+ ptr u_selfish in
    let _ = u_selfish +:+ ptr (union "other") in
    seal u_selfish;

    assert_typ_printed_as "union u_selfish {
                              union u_selfish *field_0;
                              union other *field_1;
                           }"
      u_selfish;

    (* Structs and unions containing arrays and pointers to functions *)
    let mixture = structure "mixture" in
    let _ = mixture *:* array 10 (array 12 (ptr mixture)) in
    let _ = mixture *:* funptr (ptr mixture @-> returning void) in
    let _ = mixture *:* int in
    seal mixture;
      
    assert_typ_printed_as ~name:"d" 
                          "struct mixture {
                              struct mixture *field_0[10][12];
                              void (*field_1)(struct mixture *);
                              int field_2;
                           } d"
      mixture;

    let u_mixture = union "u_mixture" in
    let _ = u_mixture +:+ float in
    let _ = u_mixture +:+ ptr (array 3
                                 (funptr
                                    (float @-> returning float))) in
    seal u_mixture;
      
    assert_typ_printed_as ~name:"e"
                          "union u_mixture {
                              float field_0;
                              float (*(*field_1)[3])(float);
                           } e"
      u_mixture;
      
    (* Structs and unions containing struct and union members *)
    let inner_s = structure "inner_s" in
    let _ = inner_s *:* int in
    seal inner_s;

    let inner_u = union "inner_u" in
    let _ = inner_u +:+ int in
    seal inner_u;

    let struct_containing_struct = structure "scs" in
    let _ = struct_containing_struct *:* inner_s in
    seal struct_containing_struct;

    let union_containing_struct = union "ucs" in
    let _ = union_containing_struct +:+ inner_s in
    seal union_containing_struct;

    let struct_containing_union = structure "scu" in
    let _ = struct_containing_union *:* inner_u in
    seal struct_containing_union;

    let union_containing_union = union "ucu" in
    let _ = union_containing_union +:+ inner_u in
    seal union_containing_union;

    assert_typ_printed_as "struct scs {
                              struct inner_s field_0;
                           }"
      struct_containing_struct;

    assert_typ_printed_as ~name:"f" 
                          "union ucs {
                              struct inner_s field_0;
                           } f"
      union_containing_struct;

    assert_typ_printed_as "struct scu {
                              union inner_u field_0;
                           }"
      struct_containing_union;

    assert_typ_printed_as ~name:"g" 
                          "union ucu {
                              union inner_u field_0;
                           } g"
      union_containing_union;
  end


(*
  Test the printing of array types.
*)
let test_array_printing () =
  begin
    assert_typ_printed_as ~name:"a" "int a[10]"
      (array 10 int);

    assert_typ_printed_as "long [1][2][3]"
      (array 1 (array 2 (array 3 long)));

    assert_typ_printed_as ~name:"b" "int (*b[10])(float)"
      (array 10 (funptr (float @-> returning int)));

    let s = structure "s" in

    assert_typ_printed_as ~name:"c" "struct s (*(*(*c[1])[2])(int (*)[3]))[4]"
      (array 1
         (ptr (array 2
                 (funptr (ptr (array 3 int) @->
                          returning (ptr (array 4 s)))))));
  end


(*
  Test the printing of function types.
*)
let test_function_printing () =
  begin
    assert_fn_printed_as ~name:"a" "void a(void)"
      (void @-> returning void);

    assert_fn_printed_as "float(int, char, double)"
      (int @-> char @-> double @-> returning float);

    assert_fn_printed_as ~name:"c" "int (*c(void (*)(void)))(int)"
      (funptr (void @-> returning void) @->
       returning (funptr (int @-> returning int)));

    let s = structure "s" in
    let _ = s *:* int in
    seal s;
    
    assert_fn_printed_as "struct s(struct s)"
      (s @-> returning s);
  end


(*
  Test the printing of view types.
*)
let test_view_printing () =
  begin
    assert_typ_printed_as ~name:"a" "char *a"
      string;

    let v : unit typ = view ~read:(fun _ -> ()) ~write:(fun () () -> ())
      (funptr (void @-> returning void)) in
    
    assert_typ_printed_as "void (*)(void)"
      v
  end



let suite = "Type printing tests" >:::
  ["printing atomic types"
    >:: test_atomic_printing;

   "printing pointers"
    >:: test_pointer_printing;

   "printing structs and unions"
    >:: test_struct_and_union_printing;

   "printing arrays"
    >:: test_array_printing;

   "printing functions"
    >:: test_function_printing;

   "printing views"
    >:: test_view_printing;
  ]


let _ =
  run_test_tt_main suite
