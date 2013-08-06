(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Ctypes
open Foreign

type window = unit ptr
let window : window typ = ptr void

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
  foreign "box" (window @-> char @-> char @-> (returning void))

let cbreak =
  foreign "cbreak" (void @-> (returning int))
