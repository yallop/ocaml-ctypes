open OUnit
open Ffi.C


(*
  Call the functions

     int isisalnum(int)
     int isisalpha(int)
     int isiscntrl(int)
     int isisdigit(int)
     int isisgraph(int)
     int isislower(int)
     int isisprint(int)
     int isispunct(int)
     int isisspace(int)
     int isisupper(int)
     int isisxdigit(int)
*)
let test_isX_functions () =
  let t = Type.(int @-> returning int) in
  let isalnum = foreign "isalnum" t
  and isalpha = foreign "isalpha" t
  and iscntrl = foreign "iscntrl" t
  and isdigit = foreign "isdigit" t
  and isgraph = foreign "isgraph" t
  and islower = foreign "islower" t
  and isprint = foreign "isprint" t
  and ispunct = foreign "ispunct" t
  and isspace = foreign "isspace" t
  and isupper = foreign "isupper" t
  and isxdigit = foreign "isxdigit" t
  in
  let assert_satisfied f c =
    assert_equal true ((f (Char.code c)) <> 0)
  and assert_not_satisified f c =
    assert_equal 0 (f (Char.code c))
  in begin
    assert_satisfied isalnum 'a';
    assert_not_satisified isalnum ' ';
    
    assert_satisfied isalpha 'x';
    assert_not_satisified isalpha ';';

    assert_satisfied iscntrl '\r';
    assert_not_satisified iscntrl 'a';

    assert_satisfied isdigit '2';
    assert_not_satisified isdigit 'a';

    assert_satisfied isgraph '?';
    assert_not_satisified isgraph ' ';

    assert_satisfied islower 's';
    assert_not_satisified islower 'S';

    assert_satisfied isprint ' ';
    assert_not_satisified isprint '\b';

    assert_satisfied ispunct '.';
    assert_not_satisified ispunct 'a';

    assert_satisfied isspace '\t';
    assert_not_satisified isspace '~';

    assert_satisfied isupper 'X';
    assert_not_satisified isupper 'x';

    assert_satisfied isxdigit 'f';
    assert_not_satisified isxdigit 'g';
  end


(*
  Call the functions

    char *strchr(const char *str, int c); 
    int strcmp(const char *str1, const char *str2); 
*)
let test_string_functions () =
  let open Type in

  (* char *strchr(const char *str, int c);  *)
  let strchr = foreign "strchr" (string @-> int @-> returning string) in

  (* int strcmp(const char *str1, const char *str2);  *)
  let strcmp = foreign "strcmp" (string @-> string @-> returning int) in
  
  assert_equal "defg" (strchr "abcdefg" (Char.code 'd'))
    ~printer:(fun x -> x);

  assert_bool "strcmp('abc', 'def') < 0"
    (strcmp "abc" "def" < 0);

  assert_bool "strcmp('def', 'abc') > 0"
    (strcmp "def" "abc" > 0);

  assert_bool "strcmp('abc', 'abc') == 0"
    (strcmp "abc" "abc" = 0)

  (* TODO: test other string functions once we support size_t *)


(*
  Call the functions

     div_t div(int numerator, int denominator)

  where div_t is defined as follows:

    typedef struct
      {
        int quot;			/* Quotient.  */
        int rem;			/* Remainder.  */
      } div_t;
*)
let test_div () =
  let module M = struct
    open Struct
    open Type
    type div_t
    let div_t : div_t structure typ = tag "div_t"
    let quot = div_t *:* int
    let rem = div_t *:* int
    let () = seal div_t
      
    let div = foreign "div" (int @-> int @-> returning div_t)

    let test ~num ~dem ~quotient ~remainder =
      let v = div num dem in
      let () = assert_equal quotient (getf v quot) in
      let () = assert_equal remainder (getf v rem) in
      ()
      
    let () = test ~num:10 ~dem:2 ~quotient:5 ~remainder:0
      
    let () = test ~num:11 ~dem:2 ~quotient:5 ~remainder:1
  end in ()

let suite = "C standard library tests" >:::
  ["test isX functions" >:: test_isX_functions;
   "test string function" >:: test_string_functions;
   "test div function" >:: test_div;
  ]


let _ =
  run_test_tt_main suite
