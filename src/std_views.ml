(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Static
open Dynamic

let string_of_char_ptr {raw_ptr; pbyte_offset} =
  Std_view_stubs.string_of_cstring raw_ptr pbyte_offset

let char_ptr_of_string s =
  let buf = Std_view_stubs.cstring_of_string s in
  { reftype = char;
    pmanaged = Some buf;
    raw_ptr = Dynamic_stubs.block_address buf;
    pbyte_offset = 0 }

let string = view (ptr char)
  ~read:string_of_char_ptr ~write:char_ptr_of_string

let castp typ p = from_voidp typ (to_voidp p)

let read_nullable t p =
  if p = null then None
  else Some !@(castp t (allocate (ptr void) p))

let write_nullable t = function
  | None -> null
  | Some f -> !@(castp (ptr void) (allocate t f))

let nullable_view t =
  let read = read_nullable t
  and write = write_nullable t in
  view ~read ~write (ptr void)

let funptr_opt fn = nullable_view (funptr fn)

let ptr_opt t = nullable_view (ptr t)

let string_opt = nullable_view string
