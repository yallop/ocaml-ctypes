open OUnit
open Ctypes


let testlib = Dl.(dlopen ~filename:"clib/test_functions.so" ~flags:[RTLD_NOW])


(*
  Call a function of type

     void (char **sv, int sc, char *buffer)

  using strings for input parameters and a char array for an output
  parameter.  Examine the output buffer using a cast to a string view.
*)
let test_passing_string_array () =
  let concat = foreign "concat_strings" ~from:testlib
    (ptr string @-> int @-> ptr char @-> returning void) in

  let l = ["the "; "quick "; "brown "; "fox "; "etc. "; "etc. "; ] in
  let arr = Array.of_list string l in

  let outlen = List.fold_left (fun a s -> String.length s + a) 1 l in
  let buf = Array.make char outlen in

  let () = Array.(concat (start arr) (length arr) (start buf)) in
  let buf_addr = Ptr.make (ptr char) (Array.start buf) in
  let s = Ptr.from_voidp string (Ptr.to_voidp buf_addr) in

  assert_equal ~msg:"Check output"
    "the quick brown fox etc. etc. " Ptr.(!s)


(*
  Call a function of type

     int (int)

  using a custom view that treats chars as ints.
*)
let test_passing_chars_as_ints () =
  let charish = view ~read:Char.chr ~write:Char.code int in
  let toupper = foreign "toupper" (charish @-> returning charish) in

  assert_equal ~msg:"toupper('x') = 'X'"
    'X' (toupper 'x');

  assert_equal ~msg:"toupper('3') = '3'"
    '3' (toupper '3');

  assert_equal ~msg:"toupper('X') = 'X'"
    'X' (toupper 'X')



(*
  Use views to create a nullable function pointer.
*)
let test_nullable_function_pointer_view () =
  let open Ptr in
  let read_nullable : unit ptr -> (int -> int -> int) option =
    fun p ->
    if p = null then None
    else (* rather circuitous *)
      (* void **pp = malloc(sizeof *pp); *pp = p; *)
      let pp = Ptr.make (ptr void) p in
      (* (int ( **fp)(int, int)) = (int ( ** )(int, int)) p *)
      let fpp =
        from_voidp (funptr (int @-> int @-> returning int)) (to_voidp pp) in
      Some !fpp
  and write_nullable  : (int -> int -> int) option -> unit ptr = function
    | None -> null
    | Some f ->
      (* (int ( **fp)(int, int)) = malloc(sizeof *fp); *fp = p *)
      let fp : (int -> int -> int) ptr = Ptr.make (funptr (int @-> int @-> returning int)) f in
      (* *((void ** )fp) *)
      !(from_voidp (ptr void) (to_voidp fp))
  in
  let nullable_intptr = view ~read:read_nullable ~write:write_nullable (ptr void) in
  let returning_funptr =
    foreign "returning_funptr" ~from:testlib
      (int @-> returning nullable_intptr)
  and accepting_possibly_null_funptr =
    foreign "accepting_possibly_null_funptr" ~from:testlib
      (nullable_intptr @-> int @-> int @-> returning int)
  in
  begin
    let fromSome = function None -> assert false | Some x -> x in

    let add = fromSome (returning_funptr 0)
    and times = fromSome (returning_funptr 1) in

    assert_equal ~msg:"reading non-null function pointer return value"
      9 (add 5 4);

    assert_equal ~msg:"reading non-null function pointer return value"
      20 (times 5 4);

    assert_equal ~msg:"reading null function pointer return value"
      None (returning_funptr 2);

    assert_equal ~msg:"passing null function pointer"
      (-1) (accepting_possibly_null_funptr None 2 3);

    assert_equal ~msg:"passing non-null function pointer"
      5 (accepting_possibly_null_funptr (Some Pervasives.(+)) 2 3);

    assert_equal ~msg:"passing non-null function pointer obtained from C"
      6 (accepting_possibly_null_funptr (returning_funptr 1) 2 3);

  end


let suite = "View tests" >:::
  ["passing array of strings" >:: test_passing_string_array;
   "custom views" >:: test_passing_chars_as_ints;
   "nullable function pointers" >:: test_nullable_function_pointer_view;
  ]


let _ =
  run_test_tt_main suite
