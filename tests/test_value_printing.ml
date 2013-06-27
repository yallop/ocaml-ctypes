(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open OUnit
open Ctypes


let testlib = Dl.(dlopen ~filename:"clib/test_functions.so" ~flags:[RTLD_NOW])

let strip_whitespace = Str.(global_replace (regexp "[\n ]+") "")

let equal_ignoring_whitespace l r =
  strip_whitespace l = strip_whitespace r


(*
  Test the printing of atomic values: arithmetic types and values of abstract
  types.
*)
let test_atomic_printing () =
  let open Signed in
  let open Unsigned in

  (* char *)
  let retrieve_CHAR_MIN = Foreign.foreign "retrieve_CHAR_MIN" ~from:testlib
      (void @-> returning char) in
  let _CHAR_MIN = retrieve_CHAR_MIN () in
  let retrieve_CHAR_MAX = Foreign.foreign "retrieve_CHAR_MAX" ~from:testlib
      (void @-> returning char) in
  let _CHAR_MAX = retrieve_CHAR_MAX () in

  assert_equal (string_of char _CHAR_MIN) (Printf.sprintf "'%c'" _CHAR_MIN);
  assert_equal (string_of char 'a') "'a'";
  assert_equal (string_of char 'A') "'A'";
  assert_equal (string_of char '3') "'3'";
  assert_equal (string_of char '\n') "'\n'";
  assert_equal (string_of char ' ') "' '";
  assert_equal (string_of char _CHAR_MAX) (Printf.sprintf "'%c'" _CHAR_MAX);

  (* signed char *)
  let retrieve_SCHAR_MIN = Foreign.foreign "retrieve_SCHAR_MIN" ~from:testlib
      (void @-> returning schar) in
  let _SCHAR_MIN = retrieve_SCHAR_MIN () in
  let retrieve_SCHAR_MAX = Foreign.foreign "retrieve_SCHAR_MAX" ~from:testlib
      (void @-> returning schar) in
  let _SCHAR_MAX = retrieve_SCHAR_MAX () in

  assert_equal (string_of schar _SCHAR_MIN) (string_of_int _SCHAR_MIN);
  assert_equal (string_of schar 0) (string_of_int 0);
  assert_equal (string_of schar (-5)) (string_of_int (-5));
  assert_equal (string_of schar 5) (string_of_int 5);
  assert_equal (string_of schar _SCHAR_MAX) (string_of_int _SCHAR_MAX);

  (* short *)
  let retrieve_SHRT_MIN = Foreign.foreign "retrieve_SHRT_MIN" ~from:testlib
    (void @-> returning short) in
  let _SHRT_MIN = retrieve_SHRT_MIN () in
  let retrieve_SHRT_MAX = Foreign.foreign "retrieve_SHRT_MAX" ~from:testlib
    (void @-> returning short) in
  let _SHRT_MAX = retrieve_SHRT_MAX () in

  assert_equal (string_of short _SHRT_MIN) (string_of_int _SHRT_MIN);
  assert_equal (string_of short 0) (string_of_int 0);
  assert_equal (string_of short (-5)) (string_of_int (-5));
  assert_equal (string_of short 14) (string_of_int 14);
  assert_equal (string_of short _SHRT_MAX) (string_of_int _SHRT_MAX);

  (* int *)
  let retrieve_INT_MIN = Foreign.foreign "retrieve_INT_MIN" ~from:testlib
      (void @-> returning int) in
  let _INT_MIN = retrieve_INT_MIN () in
  let retrieve_INT_MAX = Foreign.foreign "retrieve_INT_MAX" ~from:testlib
      (void @-> returning int) in
  let _INT_MAX = retrieve_INT_MAX () in

  assert_equal (string_of int _INT_MIN) (string_of_int _INT_MIN);
  assert_equal (string_of int 0) (string_of_int 0);
  assert_equal (string_of int (-5)) (string_of_int (-5));
  assert_equal (string_of int 14) (string_of_int 14);
  assert_equal (string_of int _INT_MAX) (string_of_int _INT_MAX);

  (* long *)
  let retrieve_LONG_MAX = Foreign.foreign "retrieve_LONG_MAX" ~from:testlib
      (void @-> returning long) in
  let _LONG_MAX = retrieve_LONG_MAX () in
  let retrieve_LONG_MIN = Foreign.foreign "retrieve_LONG_MIN" ~from:testlib
      (void @-> returning long) in
  let _LONG_MIN = retrieve_LONG_MIN () in

  assert_equal (string_of long _LONG_MIN) Long.(to_string _LONG_MIN);
  assert_equal (string_of long Long.(of_int 0)) Long.(to_string (of_int 0));
  assert_equal (string_of long (Long.of_int (-5))) Long.(to_string (of_int (-5)));
  assert_equal (string_of long (Long.of_int 14)) Long.(to_string (of_int 14));
  assert_equal (string_of long _LONG_MAX) Long.(to_string _LONG_MAX);

  (* long long *)
  let retrieve_LLONG_MAX = Foreign.foreign "retrieve_LLONG_MAX" ~from:testlib
      (void @-> returning llong) in
  let _LLONG_MAX = retrieve_LLONG_MAX () in
  let retrieve_LLONG_MIN = Foreign.foreign "retrieve_LLONG_MIN" ~from:testlib
      (void @-> returning llong) in
  let _LLONG_MIN = retrieve_LLONG_MIN () in

  assert_equal (string_of llong _LLONG_MIN) LLong.(to_string _LLONG_MIN);
  assert_equal (string_of llong LLong.(of_int 0)) LLong.(to_string (of_int 0));
  assert_equal (string_of llong (LLong.of_int (-5))) LLong.(to_string (of_int (-5)));
  assert_equal (string_of llong (LLong.of_int 14)) LLong.(to_string (of_int 14));
  assert_equal (string_of llong _LLONG_MAX) LLong.(to_string _LLONG_MAX);

  (* unsigned char *)
  let retrieve_UCHAR_MAX = Foreign.foreign "retrieve_UCHAR_MAX" ~from:testlib
    (void @-> returning uchar) in
  let _UCHAR_MAX = retrieve_UCHAR_MAX () in

  UChar.(assert_equal (string_of uchar (of_int 0)) (to_string (of_int 0)));
  UChar.(assert_equal (string_of uchar (of_int 5)) (to_string (of_int 5)));
  UChar.(assert_equal (string_of uchar _UCHAR_MAX) (to_string _UCHAR_MAX));

  (* unsigned short *)
  let retrieve_USHRT_MAX = Foreign.foreign "retrieve_USHRT_MAX" ~from:testlib
      (void @-> returning ushort) in
  let _USHRT_MAX = retrieve_USHRT_MAX () in

  UShort.(assert_equal (string_of ushort (of_int 0)) (to_string (of_int 0)));
  UShort.(assert_equal (string_of ushort (of_int 5)) (to_string (of_int 5)));
  UShort.(assert_equal (string_of ushort _USHRT_MAX) (to_string _USHRT_MAX));

  (* unsigned int *)
  let retrieve_UINT_MAX = Foreign.foreign "retrieve_UINT_MAX" ~from:testlib
      (void @-> returning uint) in
  let _UINT_MAX = retrieve_UINT_MAX () in

  UInt.(assert_equal (string_of uint (of_int 0)) (to_string (of_int 0)));
  UInt.(assert_equal (string_of uint (of_int 5)) (to_string (of_int 5)));
  UInt.(assert_equal (string_of uint _UINT_MAX) (to_string _UINT_MAX));

  (* unsigned long *)
  let retrieve_ULONG_MAX = Foreign.foreign "retrieve_ULONG_MAX" ~from:testlib
      (void @-> returning ulong) in
  let _ULONG_MAX = retrieve_ULONG_MAX () in

  ULong.(assert_equal (string_of ulong (of_int 0)) (to_string (of_int 0)));
  ULong.(assert_equal (string_of ulong (of_int 5)) (to_string (of_int 5)));
  ULong.(assert_equal (string_of ulong _ULONG_MAX) (to_string _ULONG_MAX));

  (* unsigned long long *)
  let retrieve_ULLONG_MAX = Foreign.foreign "retrieve_ULLONG_MAX" ~from:testlib
      (void @-> returning ullong) in
  let _ULLONG_MAX = retrieve_ULLONG_MAX () in

  ULLong.(assert_equal (string_of ullong (of_int 0)) (to_string (of_int 0)));
  ULLong.(assert_equal (string_of ullong (of_int 5)) (to_string (of_int 5)));
  ULLong.(assert_equal (string_of ullong _ULLONG_MAX) (to_string _ULLONG_MAX));

  (* int8_t *)
  let retrieve_INT8_MIN = Foreign.foreign "retrieve_INT8_MIN" ~from:testlib
    (void @-> returning int8_t) in
  let _INT8_MIN = retrieve_INT8_MIN () in
  let retrieve_INT8_MAX = Foreign.foreign "retrieve_INT8_MAX" ~from:testlib
    (void @-> returning int8_t) in
  let _INT8_MAX = retrieve_INT8_MAX () in

  assert_equal (string_of int8_t _INT8_MIN) (string_of_int _INT8_MIN);
  assert_equal (string_of int8_t 0) (string_of_int 0);
  assert_equal (string_of int8_t (-5)) (string_of_int (-5));
  assert_equal (string_of int8_t 14) (string_of_int 14);
  assert_equal (string_of int8_t _INT8_MAX) (string_of_int _INT8_MAX);

  (* int16_t *)
  let retrieve_INT16_MIN = Foreign.foreign "retrieve_INT16_MIN" ~from:testlib
    (void @-> returning int16_t) in
  let _INT16_MIN = retrieve_INT16_MIN () in
  let retrieve_INT16_MAX = Foreign.foreign "retrieve_INT16_MAX" ~from:testlib
    (void @-> returning int16_t) in
  let _INT16_MAX = retrieve_INT16_MAX () in

  assert_equal (string_of int16_t _INT16_MIN) (string_of_int _INT16_MIN);
  assert_equal (string_of int16_t 0) (string_of_int 0);
  assert_equal (string_of int16_t (-5)) (string_of_int (-5));
  assert_equal (string_of int16_t 14) (string_of_int 14);
  assert_equal (string_of int16_t _INT16_MAX) (string_of_int _INT16_MAX);

  (* int32_t *)
  let retrieve_INT32_MIN = Foreign.foreign "retrieve_INT32_MIN" ~from:testlib
    (void @-> returning int32_t) in
  let _INT32_MIN = retrieve_INT32_MIN () in
  let retrieve_INT32_MAX = Foreign.foreign "retrieve_INT32_MAX" ~from:testlib
    (void @-> returning int32_t) in
  let _INT32_MAX = retrieve_INT32_MAX () in

  assert_equal (string_of int32_t _INT32_MIN) (Int32.to_string _INT32_MIN);
  assert_equal (string_of int32_t 0l) (Int32.to_string 0l);
  assert_equal (string_of int32_t (-5l)) (Int32.to_string (-5l));
  assert_equal (string_of int32_t 14l) (Int32.to_string 14l);
  assert_equal (string_of int32_t _INT32_MAX) (Int32.to_string _INT32_MAX);

  (* int64_t *)
  let retrieve_INT64_MIN = Foreign.foreign "retrieve_INT64_MIN" ~from:testlib
    (void @-> returning int64_t) in
  let _INT64_MIN = retrieve_INT64_MIN () in
  let retrieve_INT64_MAX = Foreign.foreign "retrieve_INT64_MAX" ~from:testlib
    (void @-> returning int64_t) in
  let _INT64_MAX = retrieve_INT64_MAX () in

  assert_equal (string_of int64_t _INT64_MIN) (Int64.to_string _INT64_MIN);
  assert_equal (string_of int64_t 0L) (Int64.to_string 0L);
  assert_equal (string_of int64_t (-5L)) (Int64.to_string (-5L));
  assert_equal (string_of int64_t 14L) (Int64.to_string 14L);
  assert_equal (string_of int64_t _INT64_MAX) (Int64.to_string _INT64_MAX);

  (* uint8_t *)
  let retrieve_UINT8_MAX = Foreign.foreign "retrieve_UINT8_MAX" ~from:testlib
      (void @-> returning uint8_t) in
  let _UINT8_MAX = retrieve_UINT8_MAX () in

  UInt8.(assert_equal (string_of uint8_t (of_int 0)) (to_string (of_int 0)));
  UInt8.(assert_equal (string_of uint8_t (of_int 5)) (to_string (of_int 5)));
  UInt8.(assert_equal (string_of uint8_t _UINT8_MAX) (to_string _UINT8_MAX));

  (* uint16_t *)
  let retrieve_UINT16_MAX = Foreign.foreign "retrieve_UINT16_MAX" ~from:testlib
      (void @-> returning uint16_t) in
  let _UINT16_MAX = retrieve_UINT16_MAX () in

  UInt16.(assert_equal (string_of uint16_t (of_int 0)) (to_string (of_int 0)));
  UInt16.(assert_equal (string_of uint16_t (of_int 5)) (to_string (of_int 5)));
  UInt16.(assert_equal (string_of uint16_t _UINT16_MAX) (to_string _UINT16_MAX));

  (* uint32_t *)
  let retrieve_UINT32_MAX = Foreign.foreign "retrieve_UINT32_MAX" ~from:testlib
      (void @-> returning uint32_t) in
  let _UINT32_MAX = retrieve_UINT32_MAX () in

  UInt32.(assert_equal (string_of uint32_t (of_int 0)) (to_string (of_int 0)));
  UInt32.(assert_equal (string_of uint32_t (of_int 5)) (to_string (of_int 5)));
  UInt32.(assert_equal (string_of uint32_t _UINT32_MAX) (to_string _UINT32_MAX));

  (* uint64_t *)
  let retrieve_UINT64_MAX = Foreign.foreign "retrieve_UINT64_MAX" ~from:testlib
      (void @-> returning uint64_t) in
  let _UINT64_MAX = retrieve_UINT64_MAX () in

  UInt64.(assert_equal (string_of uint64_t (of_int 0)) (to_string (of_int 0)));
  UInt64.(assert_equal (string_of uint64_t (of_int 5)) (to_string (of_int 5)));
  UInt64.(assert_equal (string_of uint64_t _UINT64_MAX) (to_string _UINT64_MAX));

  (* size_t *)
  let retrieve_SIZE_MAX = Foreign.foreign "retrieve_SIZE_MAX" ~from:testlib
      (void @-> returning size_t) in
  let _SIZE_MAX = retrieve_SIZE_MAX () in

  Size_t.(assert_equal (string_of size_t (of_int 0)) (to_string (of_int 0)));
  Size_t.(assert_equal (string_of size_t (of_int 5)) (to_string (of_int 5)));
  Size_t.(assert_equal (string_of size_t _SIZE_MAX) (to_string _SIZE_MAX));

  (* nativeint *)
  let retrieve_nINT_MIN = Foreign.foreign "retrieve_INT_MIN" ~from:testlib
      (void @-> returning nativeint) in
  let _nINT_MIN = retrieve_nINT_MIN () in
  let retrieve_nINT_MAX = Foreign.foreign "retrieve_INT_MAX" ~from:testlib
      (void @-> returning nativeint) in
  let _nINT_MAX = retrieve_nINT_MAX () in

  assert_equal (string_of nativeint _nINT_MIN) (Nativeint.to_string _nINT_MIN);
  assert_equal (string_of nativeint 0n) (Nativeint.to_string 0n);
  assert_equal (string_of nativeint (-5n)) (Nativeint.to_string (-5n));
  assert_equal (string_of nativeint 14n) (Nativeint.to_string 14n);
  assert_equal (string_of nativeint _nINT_MAX) (Nativeint.to_string _nINT_MAX);

  (* float *)
  let retrieve_FLT_MIN = Foreign.foreign "retrieve_FLT_MIN" ~from:testlib
      (void @-> returning float) in
  let _FLT_MIN = retrieve_FLT_MIN () in
  let retrieve_FLT_MAX = Foreign.foreign "retrieve_FLT_MAX" ~from:testlib
      (void @-> returning float) in
  let _FLT_MAX = retrieve_FLT_MAX () in

  assert_equal (string_of float _FLT_MIN) (string_of_float _FLT_MIN);
  assert_equal (valid_float_lexem (string_of float 0.0)) (string_of_float 0.0);
  assert_equal (string_of float nan) (string_of_float nan);
  assert_equal (string_of float infinity) (string_of_float infinity);
  assert_equal (string_of float _FLT_MAX) (string_of_float _FLT_MAX);

  (* double *)
  let retrieve_DBL_MIN = Foreign.foreign "retrieve_DBL_MIN" ~from:testlib
      (void @-> returning double) in
  let _DBL_MIN = retrieve_DBL_MIN () in
  let retrieve_DBL_MAX = Foreign.foreign "retrieve_DBL_MAX" ~from:testlib
      (void @-> returning double) in
  let _DBL_MAX = retrieve_DBL_MAX () in

  assert_equal (string_of double _DBL_MIN) (string_of_float _DBL_MIN);
  assert_equal (valid_float_lexem (string_of double 0.0)) (string_of_float 0.0);
  assert_equal (string_of double (-1.03)) (string_of_float (-1.03));
  assert_equal (string_of double (34.22)) (string_of_float (34.22));
  assert_equal (string_of double (1.39e16)) (string_of_float (1.39e16));
  assert_equal (string_of double nan) (string_of_float nan);
  assert_equal (string_of double infinity) (string_of_float infinity);
  assert_equal (string_of double _DBL_MAX) (string_of_float _DBL_MAX);

  ()

(*
  Test the printing of pointers.
*)
let test_pointer_printing () =
  (* There's not much we can test here, since pointer formatting is
     implementation-dependent.  We can at least run the pointer-formatting
     code, and test that pointers of different types are printed
     equivalently. *)
  let arr = Array.make int 10 in
  let p = Array.start arr in

  assert_equal
    (string_of (ptr (reference_type p)) p)
    (string_of (ptr void) (to_voidp p))
    

(*
  Test the printing of structs.
*)
let test_struct_printing () =
  let s = structure "s" in
  let a = s *:* array 3 int in
  let d = s *:* double in
  let f = s *:* funptr (int @-> returning int) in
  let () = seal s in

  let t = structure "t" in
  let ts = t *:* s in
  let ti = t *:* int in
  let () = seal t in

  let vt = make t in
  let vs = make s in

  begin
    setf vs a (Array.of_list int [4; 5; 6]);
    setf vs d nan;
    setf vs f succ;

    setf vt ts vs;
    setf vt ti 14;

    assert_bool "struct printing"
      (equal_ignoring_whitespace "{{ {4, 5, 6}, nan, <fun> }, 14}"
         (string_of t vt))
  end


(*
  Test the printing of unions.
*)
let test_union_printing () =
  let s = structure "s" in
  let i = s *:* uint16_t in
  let j = s *:* uint16_t in
  let () = seal s in
  let u = union "u" in
  let us = u +:+ s in
  let ua = u +:+ array 4 uint8_t in
  let () = seal u in
  let v = make u in
  setf v ua (Array.make ~initial:(Unsigned.UInt8.of_int 0) uint8_t 4);
  assert_bool "union printing"
    (equal_ignoring_whitespace "{{0, 0} | {0, 0, 0, 0}}" (string_of u v))


(*
  Test the printing of array types.
*)
let test_array_printing () =
  let arr = Array.of_list int [-1; 0; 1] in
  let arrarr = Array.of_list (array 3 int) [arr; arr] in
  assert_bool "array printing"
    (equal_ignoring_whitespace "{{-1, 0, 1}, {-1, 0, 1}}"
       (string_of (array 2 (array 3 int)) arrarr))


(*
  Test the printing of function types.
*)
let test_function_printing () =
  assert_equal
    "<fun>"
    (string_of (funptr (int @-> returning int)) succ)


let suite = "Value printing tests" >:::
  ["printing atomic values"
    >:: test_atomic_printing;

   "printing pointers"
    >:: test_pointer_printing;

   "printing structs"
    >:: test_struct_printing;

   "printing unions"
    >:: test_union_printing;

   "printing arrays"
    >:: test_array_printing;

   "printing functions"
    >:: test_function_printing;
  ]


let _ =
  run_test_tt_main suite
