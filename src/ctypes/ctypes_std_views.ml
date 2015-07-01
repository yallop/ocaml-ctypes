(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

let string_of_char_ptr (Ctypes_static.CPointer p) =
  Ctypes_std_view_stubs.string_of_cstring p

let char_ptr_of_string s =
  let managed = Ctypes_std_view_stubs.cstring_of_string s in
  Ctypes_static.CPointer (Ctypes_ptr.Fat.make ~managed ~reftyp:Ctypes_static.char
                     (Ctypes_memory_stubs.block_address managed))

let string = Ctypes_static.(view (ptr char))
  ~read:string_of_char_ptr ~write:char_ptr_of_string

let read_nullable t reftyp =
  let coerce = Ctypes_coerce.coerce Ctypes_static.(ptr reftyp) t in
  fun p -> Ctypes_memory.(if to_voidp p = null then None else Some (coerce p))

let write_nullable t reftyp =
  let coerce = Ctypes_coerce.coerce t Ctypes_static.(ptr reftyp) in
  Ctypes_memory.(function None -> from_voidp reftyp null | Some f -> coerce f)

let nullable_view ?format_typ ?format t reftyp =
  let read = read_nullable t reftyp
  and write = write_nullable t reftyp
  in Ctypes_static.(view ~read ~write ?format_typ ?format (ptr reftyp))

let read_nullable_funptr t reftyp =
  let coerce = Ctypes_coerce.coerce (Ctypes_static.static_funptr reftyp) t in
  fun (Ctypes_static.Static_funptr p as ptr) ->
    if Ctypes_ptr.Fat.is_null p
    then None
    else Some (coerce ptr)

let write_nullable_funptr t reftyp =
  let coerce = Ctypes_coerce.coerce t Ctypes_static.(static_funptr reftyp) in
  function None -> Ctypes_static.Static_funptr
                     (Ctypes_ptr.Fat.make ~reftyp Ctypes_ptr.Raw.null)
         | Some f -> coerce f

let nullable_funptr_view ?format_typ ?format t reftyp =
  let read = read_nullable_funptr t reftyp
  and write = write_nullable_funptr t reftyp
  in Ctypes_static.(view ~read ~write ?format_typ ?format (static_funptr reftyp))

let ptr_opt t = nullable_view (Ctypes_static.ptr t) t

let string_opt = nullable_view string Ctypes_static.char
