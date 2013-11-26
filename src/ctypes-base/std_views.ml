(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

let string_of_char_ptr {Static.raw_ptr; pbyte_offset} =
  Std_view_stubs.string_of_cstring raw_ptr pbyte_offset

let char_ptr_of_string s =
  let buf = Std_view_stubs.cstring_of_string s in
  { Static.reftype = Static.char;
    pmanaged = Some (Obj.repr buf);
    raw_ptr = Memory_stubs.block_address buf;
    pbyte_offset = 0 }

let string = Static.(view (ptr char))
  ~read:string_of_char_ptr ~write:char_ptr_of_string

let castp typ p = Memory.(from_voidp typ (to_voidp p))

let read_nullable t =
  let coerce = Coerce.coerce Static.(ptr void) t in
  fun p -> Memory.(if p = null then None else Some (coerce p))

let write_nullable t =
  let coerce = Coerce.coerce t Static.(ptr void) in
  Memory.(function None -> null | Some f -> coerce f)

let nullable_view t =
  let read = read_nullable t
  and write = write_nullable t in
  Static.(view ~read ~write (ptr void))

let ptr_opt t = nullable_view (Static.ptr t)

let string_opt = nullable_view string
