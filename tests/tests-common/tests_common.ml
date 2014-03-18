(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Functions for test stub generation. *)

open Ctypes

let filenames argv =
  let usage = "arguments: [--ml-file $filename] [--c-file $filename]"    in
  let ml_filename = ref ""
  and c_filename = ref "" in
  let spec = Arg.([ ("--ml-file", Set_string ml_filename, "ML filename");
                    ("--c-file", Set_string c_filename, "C filename"); ]) in
  let no_positional_args _ =
    prerr_endline "No positional arguments" in
  begin
    Arg.parse spec no_positional_args usage;
    (!ml_filename, !c_filename)
  end

module Foreign_binder : Cstubs.FOREIGN with type 'a fn = 'a =
struct
  type 'a fn = 'a
  let foreign name fn = Foreign.foreign name fn
end

module type STUBS = functor  (F : Cstubs.FOREIGN) -> sig end

let with_open_formatter filename f =
  let out = open_out filename in
  let fmt = Format.formatter_of_out_channel out in
  let close_channel () = close_out out in
  try
    let rv = f fmt in
    close_channel ();
    rv
  with e ->
    close_channel ();
    raise e

let header = "\
#include \"clib/test_functions.h\"
#include \"cstubs/cstubs_internals.h\"
"

let run ?(cheader="") argv specs =
  let ml_filename, c_filename = filenames argv in
  if ml_filename <> "" then
    with_open_formatter ml_filename
      (fun fmt -> Cstubs.write_ml fmt ~prefix:"cstubs_tests" specs);
  if c_filename <> "" then
    with_open_formatter c_filename
      (fun fmt -> 
        Format.fprintf fmt "%s\n%s\n" header cheader;
        Cstubs.write_c fmt ~prefix:"cstubs_tests" specs)
