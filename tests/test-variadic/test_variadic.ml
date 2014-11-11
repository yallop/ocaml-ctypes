(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Tests for binding variadic functions. *)

open OUnit2
open Ctypes


module Common_tests(S : Tests_common.FOREIGN with type 'a fn = 'a) =
struct
  module M = Functions.Stubs(S)
  open M

  (*
    Test calling snprintf.
  *)
  let test_snprintf _ =
    let bufsz = 128 in
    let write snprintf apply =
      let buf = allocate_n char bufsz in
      apply (snprintf buf bufsz);
      coerce (ptr char) string buf
    in
    begin
      assert_equal "an int: 100."
        (write snprintf_int
           (fun k -> k "an int: %d." 100));
      
      assert_equal "a char A and a float 0.0."
        (write snprintf_char_float
           (fun k -> k "a char %c and a float %.1f." 'A' 0.0));

      assert_equal "a string abcde and a short ffd."
        (write snprintf_string_short
           (fun k -> k "a string %s and a short %hx."
             "abcde" 0xffd));
    end
end


let suite = "Variadic tests" >:::
  []


let _ =
  run_test_tt_main suite
