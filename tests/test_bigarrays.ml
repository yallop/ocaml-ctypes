(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

module Std_array = Array
type 'a std_array = 'a array

open OUnit
open Ctypes
module BA = Bigarray


let testlib = Dl.(dlopen ~filename:"clib/test_functions.so" ~flags:[RTLD_NOW])


let array_of_list2 typ list2 =
  let dim2 = List.length (List.hd list2) in
  let atyp = array dim2 typ in
  Array.of_list atyp (List.map (Array.of_list typ) list2)

let array_of_list3 typ list3 =
  let dim2 = List.length (List.hd list3)
  and dim3 = List.length (List.hd (List.hd list3)) in
  let atyp = array dim2 (array dim3 typ) in
  Array.of_list atyp (List.map (array_of_list2 typ) list3)

let castp typ p = from_voidp typ (to_voidp p)


(*
  View ctypes-managed memory through a bigarray lens.
*)
let test_bigarray_of_ctypes_array () =
  (* One-dimensional Genarrays *)
  let a1 = Array.of_list int8_t [10; 20; 30; 40] in
  let b1 = bigarray_of_array genarray BA.int8_signed a1 in
  let () = begin
    assert_equal (Array.length a1) (BA.Genarray.nth_dim b1 0);
    for i = 0 to Array.length a1 - 1 do
      assert_equal a1.(i) (BA.Genarray.get b1 [|i|])
    done
  end in

  (* Array1 *)
  let eps32 = 1e-6 in
  let complex32_eq =
    let open Complex in
    fun { re = lre; im = lim } { re = rre; im = rim } ->
      abs_float (lre -. rre) < eps32 && abs_float (lim -. rim) < eps32 in
  let a2 = Array.of_list complex32
    Complex.([{re = 0.1; im = 1.0};
              {re = 0.2; im = 2.0};
              {re = 0.3; im = 3.0};
              {re = 0.4; im = 4.0}]) in
  let b2 = bigarray_of_array array1 BA.complex32 a2 in
  let () = begin
    assert_equal (Array.length a2) (BA.Array1.dim b2);
    for i = 0 to Array.length a2 - 1 do
      assert_equal a2.(i) b2.{i} ~cmp:complex32_eq
    done
  end in

  (* Two-dimensional Genarrays *)
  let uint16 = view uint16_t
    ~read:Unsigned.UInt16.to_int ~write:Unsigned.UInt16.of_int in
  let a3 = array_of_list2 uint16
    [[5; 10; 15];
     [3; 6; 9];
     [2; 4; 6];
     [1; 2; 3]] in
  let b3 = BA.reshape (bigarray_of_array genarray BA.int16_unsigned
                         (Array.from_ptr
                            (castp uint16 (Array.start a3)) 12))
    [| 4; 3 |] in
  let () = begin
    assert_equal (Array.length a3) (BA.Genarray.nth_dim b3 0);
    assert_equal (Array.length a3.(0)) (BA.Genarray.nth_dim b3 1);
    for i = 0 to Array.length a3 - 1 do
      for j = 0 to Array.length a3.(0) - 1 do
        assert_equal a3.(i).(j) (BA.Genarray.get b3 [|i; j|])
      done
    done
  end in

  (* Array2 *)
  let a4 = array_of_list2 nativeint [[5n; 10n]; [3n; 6n]; [1n; 2n]] in
  let b4 = bigarray_of_array array2 BA.nativeint a4 in
  let () = begin
    assert_equal (Array.length a4) (BA.Array2.dim1 b4);
    assert_equal (Array.length a4.(0)) (BA.Array2.dim2 b4);
    for i = 0 to Array.length a4 - 1 do
      for j = 0 to Array.length a4.(0) - 1 do
        assert_equal a4.(i).(j) b4.{i, j}
      done
    done
  end in

  (* Three-dimensional Genarrays *)
  let a5 = array_of_list3 int64_t
    [[[1L; 2L; 3L; 4L; 5L];
      [2L; 4L; 6L; 8L; 10L]];
     [[10L; 20L; 30L; 40L; 50L];
      [20L; 40L; 60L; 80L; 100L]];
     [[100L; 200L; 300L; 400L; 500L];
      [200L; 400L; 600L; 800L; 1000L]]] in
     
  let b5 = BA.reshape
    (bigarray_of_array genarray BA.int64
       (Array.from_ptr (castp int64_t (Array.start a5)) 30)) 
    [| 3; 2; 5 |] in
  let () = begin
    assert_equal (Array.length a5) (BA.Genarray.nth_dim b5 0);
    assert_equal (Array.length a5.(0)) (BA.Genarray.nth_dim b5 1);
    assert_equal (Array.length a5.(0).(0)) (BA.Genarray.nth_dim b5 2);
    for i = 0 to Array.length a5 - 1 do
      for j = 0 to Array.length a5.(0) - 1 do
        for k = 0 to Array.length a5.(0).(0) - 1 do
          assert_equal a5.(i).(j).(k) (BA.Genarray.get b5 [|i; j; k|])
        done
      done
    done
  end in

  (* Array3 *)
  let a6 = array_of_list3 double
    [[[1.; 2.; 3.; 4.];
      [2.; 4.; 6.; 8.]];
     [[10.; 20.; 30.; 40.];
      [20.; 40.; 60.; 80.]];
     [[100.; 200.; 300.; 400.];
      [200.; 400.; 600.; 800.]]] in
     
  let b6 = bigarray_of_array array3 BA.float64 a6 in
  let () = begin
    assert_equal (Array.length a6) (BA.Array3.dim1 b6);
    assert_equal (Array.length a6.(0)) (BA.Array3.dim2 b6);
    assert_equal (Array.length a6.(0).(0)) (BA.Array3.dim3 b6);
    for i = 0 to Array.length a6 - 1 do
      for j = 0 to Array.length a6.(0) - 1 do
        for k = 0 to Array.length a6.(0).(0) - 1 do
          assert_equal a6.(i).(j).(k) b6.{i, j, k}
        done
      done
    done
  end in
  ()


(*
  View bigarray-managed memory through a ctypes lens
*)
let test_ctypes_array_of_bigarray () =

  (* One-dimensional Genarrays *)
  let b1_dim = 6 in
  let b1 = BA.(Genarray.create float32 c_layout) [| b1_dim |] in
  let a1 = array_of_bigarray genarray b1 in
  begin
    assert_equal (BA.Genarray.nth_dim b1 0) (Array.length a1);

    List.iteri (fun i -> BA.Genarray.set b1 [| i |])
      [ 6.; 5.; 4.; 3.; 2.; 1. ];
    
    for i = 0 to b1_dim - 1 do
      assert_equal (BA.Genarray.get b1 [| i |]) a1.(i)
    done
  end;

  (* Array1 *)
  let b2_dim = 7 in
  let b2 = BA.(Array1.create int8_unsigned c_layout) b2_dim in
  let a2 = array_of_bigarray array1 b2 in
  begin
    assert_equal (BA.Array1.dim b2) (Array.length a2);

    List.iteri (fun i ->
      fun v -> b2.{i} <- v)
      [ 2; 4; 6; 8; 10; 12; 14 ];
    
    for i = 0 to b2_dim - 1 do
      assert_equal b2.{i} a2.(i)
    done
  end;

  (* Two-dimensional Genarrays *)
  let b3_dim1 = 4 and b3_dim2 = 2 in
  let b3 = BA.(Genarray.create int16_signed c_layout) [| b3_dim1; b3_dim2 |] in
  let a3 = Array.from_ptr
    (castp (array b3_dim2 int16_t) (bigarray_start genarray b3))
    b3_dim1 in
  begin
    assert_equal (BA.Genarray.nth_dim b3 0) (Array.length a3);
    assert_equal (BA.Genarray.nth_dim b3 1) (Array.length a3.(0));

    List.iteri (fun i ->
      List.iteri (fun j ->
        BA.Genarray.set b3 [| i; j |]))
      [[-1; -2];
       [-3; -4];
       [-5; -6];
       [-7; -8]];

    for i = 0 to b3_dim1 - 1 do
      for j = 0 to b3_dim2 - 1 do
        assert_equal (BA.Genarray.get b3 [| i; j |]) a3.(i).(j)
      done
    done
  end;

  (* Array2 *)
  let b4_dim1 = 3 and b4_dim2 = 4 in
  let b4 = BA.(Array2.create int32 c_layout) b4_dim1 b4_dim2 in
  let a4 = array_of_bigarray array2 b4 in
  begin
    assert_equal (BA.Array2.dim1 b4) (Array.length a4);
    assert_equal (BA.Array2.dim2 b4) (Array.length a4.(0));

    List.iteri (fun i ->
      List.iteri (fun j ->
        fun v -> b4.{i, j} <- v))
      [[17l; 15l; 13l; 11l];
       [9l; 7l; 5l; 3l];
       [1l; -1l; -3l; -5l]];

    for i = 0 to b4_dim1 - 1 do
      for j = 0 to b4_dim2 - 1 do
        assert_equal b4.{i, j} a4.(i).(j)
      done
    done
  end;

  (* Three-dimensional Genarrays *)
  let b5_dim1 = 4 and b5_dim2 = 2 and b5_dim3 = 5 in
  let b5 = BA.(Genarray.create int c_layout) [| b5_dim1; b5_dim2; b5_dim3 |] in
  let a5 = Array.from_ptr
    (castp (array b5_dim2 (array b5_dim3 camlint)) (bigarray_start genarray b5))
    b5_dim1 in
  begin
    assert_equal
      (BA.Genarray.nth_dim b5 0) (Array.length a5);
    assert_equal
      (BA.Genarray.nth_dim b5 1) (Array.length a5.(0));
    assert_equal
      (BA.Genarray.nth_dim b5 2) (Array.length a5.(0).(0));

    List.iteri (fun i ->
      List.iteri (fun j ->
        List.iteri (fun k ->
          BA.Genarray.set b5 [| i; j; k |])))
      [[[1; 2; 3; 4; 5];
        [6; 7; 8; 9; 10]];
       [[11; 12; 13; 14; 15];
        [16; 17; 18; 19; 20]];
       [[21; 22; 23; 24; 25];
        [26; 27; 28; 29; 30]];
       [[31; 32; 33; 34; 35];
        [36; 37; 38; 39; 40]]];

    for i = 0 to b5_dim1 - 1 do
      for j = 0 to b5_dim2 - 1 do
        for k = 0 to b5_dim3 - 1 do
          assert_equal
            (BA.Genarray.get b5 [| i; j; k |]) a5.(i).(j).(k)
        done
      done
    done
  end;

  (* Array3 *)
  let eps64 = 1e-12 in
  let complex64_eq =
    let open Complex in
    fun { re = lre; im = lim } { re = rre; im = rim } ->
      abs_float (lre -. rre) < eps64 && abs_float (lim -. rim) < eps64 in
  let b6_dim1 = 3 and b6_dim2 = 4 and b6_dim3 = 2 in
  let b6 = BA.(Array3.create complex64 c_layout) b6_dim1 b6_dim2 b6_dim3 in
  let a6 = array_of_bigarray array3 b6 in
  begin
    assert_equal (BA.Array3.dim1 b6) (Array.length a6);
    assert_equal (BA.Array3.dim2 b6) (Array.length a6.(0));
    assert_equal (BA.Array3.dim3 b6) (Array.length a6.(0).(0));

    let open Complex in
    List.iteri (fun i ->
      List.iteri (fun j  ->
        List.iteri (fun k ->
          fun v -> b6.{i, j, k} <- v)))
        [[[{re = 1.; im = 10.}; {re = 1e2; im = 0.0}];
          [{re = 2.; im = 20.}; {re = 2e2; im = 0.0}];
          [{re = 3.; im = 30.}; {re = 3e2; im = 0.0}];
          [{re = 4.; im = 40.}; {re = 4e2; im = 0.0}]];
         
         [[{re = 5.; im = 50.}; {re = 5e2; im = 0.1}];
          [{re = 6.; im = 60.}; {re = 6e2; im = 0.1}];
          [{re = 7.; im = 70.}; {re = 7e2; im = 0.1}];
          [{re = 8.; im = 80.}; {re = 8e2; im = 0.1}]];
         
         [[{re = 9.; im = 90.}; {re = 9e2; im = 0.2}];
          [{re = 10.; im = 100.}; {re = 1e3; im = 0.2}];
          [{re = 11.; im = 110.}; {re = 1.1e3; im = 0.2}];
          [{re = 12.; im = 120.}; {re = 1.2e3; im = 0.2}]]];

    for i = 0 to b6_dim1 - 1 do
      for j = 0 to b6_dim2 - 1 do
        for k = 0 to b6_dim3 - 1 do
          assert_equal b6.{i, j, k} a6.(i).(j).(k)
            ~cmp:complex64_eq
        done
      done
    done
  end


let suite = "Bigarray tests" >:::
  [
    "View ctypes-managed memory using bigarrays"
    >:: test_bigarray_of_ctypes_array;

    "View bigarray-managed memory using ctypes"
    >:: test_ctypes_array_of_bigarray;
  ]


let _ =
  run_test_tt_main suite
