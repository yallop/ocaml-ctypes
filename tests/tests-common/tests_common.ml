(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Functions for test stub generation. *)

open Ctypes

let mk_stubname cname =
  "ctypes_test_stub" ^ cname

let cstub ~cname fmt fn =
  let stub_name = mk_stubname cname in
  Cstubs.write_c ~stub_name ~cname fmt fn

let mlstub ~cname fmt f =
  let stub_name = mk_stubname cname in
  Cstubs.write_ml ~stub_name ~external_name:cname fmt  f

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

type generator = { stub: 'a 'b. cname:string -> Format.formatter -> ('a -> 'b) Static.fn -> unit }


module type FOREIGN =
sig
  type 'a result
  val foreign : string -> ('a -> 'b) fn -> ('a -> 'b) result
end

module Foreign_binder : FOREIGN with type 'a result = 'a =
struct
  type 'a result = 'a
  let foreign name fn = Foreign.foreign name fn
end

module type STUBS = functor  (F : FOREIGN) -> sig end

let generate ?header { stub } (module Specs : STUBS) filename =
  let out = open_out filename in
  let fmt = Format.formatter_of_out_channel out in
  let close_channel () = close_out out in
  try
    begin match header with
    | None -> ()
    | Some h -> Format.pp_print_string fmt h
    end;
    let module M = Specs
      (struct
        type 'a result = unit
        let foreign cname fn = stub ~cname fmt fn
       end) in
    close_channel ()
  with e ->
    close_channel ();
    raise e

let add_cstubs_header ~cheader = "\
#include \"clib/test_functions.h\"
#include \"cstubs/cstubs_internals.h\"
" ^ cheader

let run ?(cheader="") argv specs =
  let ml_filename, c_filename = filenames argv in
  if ml_filename <> "" then
    generate { stub = mlstub } specs ml_filename;
  if c_filename <> "" then
    generate ~header:(add_cstubs_header ~cheader)
      { stub = cstub } specs c_filename
