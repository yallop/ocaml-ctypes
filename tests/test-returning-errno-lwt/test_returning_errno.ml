(*
 * Copyright (c) 2016 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open OUnit2
open Ctypes


module Bindings = Functions.Stubs(Generated_bindings)
module Constants = Types.Struct_stubs(Generated_struct_bindings)


(*
  Test the binding to "stat".
 *)
let test_stat _ =
  let s = make Constants.stat in
  begin
    Lwt_unix.run
      Lwt.((Bindings.stat "." (addr s)).lwt >>= fun (x, errno) ->
           assert_equal 0 x;
           assert_equal 0 errno;
           return ());
    Lwt_unix.run
      Lwt.((Bindings.stat "/does-not-exist" (addr s)).lwt >>= fun (x, errno) ->
           assert_equal (-1) x;
           assert_equal Constants._ENOENT errno;
           return ())
  end


let suite = "Errno tests" >:::
  ["calling stat"
    >:: test_stat;
  ]


let _ =
  run_test_tt_main suite
