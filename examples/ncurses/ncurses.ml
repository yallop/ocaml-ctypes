open Ffi.C
open Type

type window = unit ptr
let window : window t = ptr void

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

let _addstr =
  foreign "addstr" (ptr char @-> (returning void))

let addstr s =
  _addstr (char_ptr_of_string s) 

let _mvwaddstr =
  foreign "mvwaddstr" (window @-> int @-> int @-> ptr char @-> (returning void))

let mvwaddstr w y x s =
  _mvwaddstr w y x (char_ptr_of_string s)

let box =
  foreign "box" (window @-> int @-> int @-> (returning void))

let cbreak =
  foreign "cbreak" (void @-> (returning void))
