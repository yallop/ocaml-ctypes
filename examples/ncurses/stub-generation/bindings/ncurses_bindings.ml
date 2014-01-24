(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Ctypes

type window = unit ptr
let window : window typ = ptr void

module Bindings (F : sig val foreign : string -> ('a -> 'b) fn -> unit end) =
struct
  open F

  let initscr =
    foreign "initscr" (void @-> (returning window))

  let endwin =
    foreign "endwin" (void @-> (returning void))

  let refresh =
    foreign "refresh" (void @-> (returning void))

  let wrefresh =
    foreign "wrefresh" (window @-> (returning void))

  let newwin =
    foreign "newwin" (int @-> int @-> int @-> int @-> (returning window))

  let addch =
    foreign "addch" (char @-> (returning void))

  let mvwaddch =
    foreign "mvwaddch" (window @-> int @-> int @-> char @-> (returning void))

  let addstr =
    foreign "addstr" (string @-> (returning void))

  let mvwaddstr =
    foreign "mvwaddstr" (window @-> int @-> int @-> string @-> (returning void))

  let box =
    foreign "box" (window @-> int @-> int @-> (returning void))

  let cbreak =
    foreign "cbreak" (void @-> (returning void))
end

let c_headers = "
#include <ncurses.h>
#include \"cstubs/cstubs_internals.h\"
"

let make_stubname cname = "ncurses_stub_" ^ cname

let main () =
  let ml_out = open_out "examples/ncurses/stub-generation/ncurses_generated.ml"
  and c_out = open_out "examples/ncurses/stub-generation/ncurses_stubs.c" in
  let ml_fmt = Format.formatter_of_out_channel ml_out
  and c_fmt = Format.formatter_of_out_channel c_out in
  Format.fprintf c_fmt "%s@\n" c_headers;
  let module M = Bindings(struct
    let foreign cname fn =
      let stub_name = make_stubname cname in
      Cstubs.write_c ~stub_name ~cname c_fmt fn;
      Cstubs.write_ml ~stub_name ~external_name:cname ml_fmt fn
  end) in
  Format.pp_print_flush ml_fmt ();
  Format.pp_print_flush c_fmt ();
  close_out ml_out;
  close_out c_out

let () = main ()
