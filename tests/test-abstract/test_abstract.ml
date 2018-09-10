open OUnit2
open Ctypes

module Generated = Generated_struct_bindings

let suite = "Abstract type tests" >::: [
  "inferred" >:: begin fun _ ->
    let t = Generated.abstract ~name:"int" ?size:None ?alignment:None in
    assert_equal (sizeof t) (sizeof int);
    assert_equal (alignment t) (alignment int)
  end;

  "guessed" >:: begin fun _ ->
    let size = sizeof int in
    let alignment = alignment int in
    let t = Generated.abstract ~name:"int" ~size ~alignment in
    assert_equal (sizeof t) size;
    assert_equal (Ctypes.alignment t) alignment
  end;

  "wrong size" >:: begin fun _ ->
    let size = sizeof int in
    let expected_message =
      Printf.sprintf
        "Inferred size for int is %i, but ~size:%i was passed to Ctypes.abstract"
        size (size + 1)
    in
    assert_raises
      (Failure expected_message)
      (fun () ->
        Generated.abstract ~name:"int" ~size:(size + 1) ?alignment:None)
  end;

  "wrong alignment" >:: begin fun _ ->
    let alignment = alignment int in
    let expected_message =
      Printf.sprintf
        "Inferred alignment for int is %i, but ~alignment:%i was passed to Ctypes.abstract"
        alignment (alignment + 1)
    in
    assert_raises
      (Failure expected_message)
      (fun () ->
        Generated.abstract ~name:"int" ?size:None ~alignment:(alignment + 1))
  end;
]

let _ =
  run_test_tt_main suite
