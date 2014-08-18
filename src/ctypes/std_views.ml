(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

let string_of_char_ptr (Static.CPointer p) =
  Std_view_stubs.string_of_cstring p

let char_ptr_of_string s =
  let managed = Std_view_stubs.cstring_of_string s in
  Static.CPointer (Ctypes_ptr.Fat.make ~managed ~reftyp:Static.char
                     (Memory_stubs.block_address managed))

let string = Static.(view (ptr char))
  ~read:string_of_char_ptr ~write:char_ptr_of_string

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
