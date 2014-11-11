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

module Foreign_binder =
struct
  type 'a fn = 'a
  let foreign name fn = Foreign.foreign name fn
end

module type FOREIGN = sig
  type 'a fn = 'a
  val foreign : string -> ('a -> 'b) Ctypes.fn -> ('a -> 'b) fn
end

module type STUBS = functor  (F : FOREIGN) -> sig end
