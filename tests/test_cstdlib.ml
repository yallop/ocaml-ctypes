open OUnit
open Ffi.C
open Unsigned


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
  let strchr_ = foreign "strchr" (ptr char @-> int @-> returning (ptr char)) in
  let strchr s c = string_of_char_ptr (strchr_ (char_ptr_of_string s) c) in

  (* int strcmp(const char *str1, const char *str2);  *)
  let strcmp_ = foreign "strcmp" (ptr char @-> ptr char @-> returning int) in
  let strcmp s1 s2 = strcmp_ (char_ptr_of_string s1) (char_ptr_of_string s2) in
  
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


(*
  Call the function

     void qsort(void *base, size_t nmemb, size_t size,
                int(*compar)(const void *, const void *));
*)
let test_qsort () =
  let open Type in
  let comparator = ptr void @-> ptr void @-> returning int in
  let qsort = foreign "qsort" (ptr void @-> size_t @-> size_t @-> funptr comparator @->
                               returning void) in

  let sortby (type a) (typ : a typ) (f : a -> a -> int) (l : a list) =
    let open Array in
    let open Size_t in
    let open Infix in
    let open Ptr in
    let arr = of_list typ l in
    let len = of_int (length arr) in
    let size = of_int (sizeof typ) in
    let cmp xp yp =
      let x = !(from_voidp typ xp)
      and y = !(from_voidp typ yp) in
      f x y
    in
    let () = qsort (to_voidp (start arr)) len size cmp in
    to_list arr
    in
    
    assert_equal
      [5; 4; 3; 2; 1]
      (sortby int (fun x y -> - (compare x y)) [3; 4; 1; 2; 5]);

    assert_equal
      ['o'; 'q'; 'r'; 's'; 't']
      (sortby char compare ['q'; 's'; 'o'; 'r'; 't'])


(*
  Call the function

     void *bsearch(const void *key, const void *base,
                   size_t nmemb, size_t size,
                   int (*compar)(const void *, const void *));
*)
let test_bsearch () =
  let module M = struct
    open Type
    let comparator = ptr void @-> ptr void @-> returning int
    let bsearch = foreign "bsearch" (ptr void @-> ptr void @-> size_t @-> size_t @->
                                     funptr comparator @->
                                     returning (ptr void))
    
    let qsort = foreign "qsort" (ptr void @-> size_t @-> size_t @-> funptr comparator @->
                                 returning void)
    let strlen = foreign "strlen" (ptr char @-> returning size_t)

    (*
      struct mi {
         int nr;
         char *name;
      } months[] = {
         { 1, "jan" }, { 2, "feb" }, { 3, "mar" }, { 4, "apr" },
         { 5, "may" }, { 6, "jun" }, { 7, "jul" }, { 8, "aug" },
         { 9, "sep" }, {10, "oct" }, {11, "nov" }, {12, "dec" }
      };
    *)
    open Struct
    type mi
    let mi = Struct.tag "mi"
    let mr   = mi *:* int
    let name = mi *:* ptr char
    let () = seal (mi : mi structure typ)

  let of_string : string -> char array =
    fun s ->
      let len = String.length s in
      let arr = Array.make char (len + 1) in
      for i = 0 to len - 1 do
        arr.(i) <- s.[i];
      done;
      arr.(len) <- '\000';
      arr
      
  let as_string : char ptr -> string =
    fun p -> 
      let len = Size_t.to_int (strlen p) in
      let s = String.create len in
      for i = 0 to len - 1 do
        s.[i] <- Ptr.(!(p + i));
      done;
      s

  let mkmi n s =
    let m = make mi in
    setf m mr n;
    setf m name (Array.start (of_string s));
    m
 
  open Ptr

  let cmpi m1 m2 =
    let mi1 = from_voidp mi m1 in
    let mi2 = from_voidp mi m2 in
    Pervasives.compare
      (as_string (!(mi1 |-> name)))
      (as_string (!(mi2 |-> name)))
      
  let months = Array.of_list mi [
    mkmi 1 "jan";
    mkmi 2 "feb";
    mkmi 3 "mar";
    mkmi 4 "apr";
    mkmi 5 "may";
    mkmi 6 "jun";
    mkmi 7 "jul";
    mkmi 8 "aug";
    mkmi 9 "sep";
    mkmi 10 "oct";
    mkmi 11 "nov";
    mkmi 12 "dec";
  ]

  let () = qsort
    (to_voidp (Array.start months))
    (Size_t.of_int (Array.length months))
    (Size_t.of_int (sizeof mi))
    cmpi
  
  let search : mi structure -> mi structure array -> mi structure option
    = fun key array ->
      let len = Size_t.of_int (Array.length array) in
      let size = Size_t.of_int (sizeof mi) in
      let r : unit ptr =
        bsearch (to_voidp (addr key)) (to_voidp (Array.start array)) len size cmpi in
      if r = null then None
      else Some (!(from_voidp mi r))

  let find_month_by_name : string -> mi structure option =
    fun s -> search (mkmi 0 s) months

  let () = match find_month_by_name "dec" with
      Some m -> assert_equal 12 (getf m mr)
    | _ -> assert false

  let () = match find_month_by_name "feb" with
      Some m -> assert_equal 2 (getf m mr)
    | _ -> assert false

  let () = match find_month_by_name "jan" with
      Some m -> assert_equal 1 (getf m mr)
    | _ -> assert false

  let () = match find_month_by_name "may" with
      Some m -> assert_equal 5 (getf m mr)
    | _ -> assert false

  let () = 
    assert_equal None (find_month_by_name "missing")

  let () = 
    assert_equal None (find_month_by_name "")

  end in ()
    
    
    
let suite = "C standard library tests" >:::
  ["test isX functions" >:: test_isX_functions;
   "test string function" >:: test_string_functions;
   "test div function" >:: test_div;
   "test qsort function" >:: test_qsort;
   "test bsearch function" >:: test_bsearch;
  ]


let _ =
  run_test_tt_main suite
