(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

exception CallToExpiredClosure = Ffi_stubs.CallToExpiredClosure

let format_function_pointer fn k fmt =
  Type_printing.format_fn' fn
    (fun fmt -> Format.fprintf fmt "(*%t)" k) fmt

let funptr ?name fn =
  let open Ffi in
  let read = function_of_pointer ?name fn
  and write = pointer_of_function fn
  and format_typ = format_function_pointer fn in
  Static.(view ~format_typ ~read ~write (ptr void))


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

let read_nullable t p = Memory.(
  if p = null then None
  else Some !@(castp t (allocate Static.(ptr void) p)))

let write_nullable t = Memory.(function
  | None -> null
  | Some f -> !@(castp Static.(ptr void) (allocate t f)))

let nullable_view t =
  let read = read_nullable t
  and write = write_nullable t in
  Static.(view ~read ~write (ptr void))

let funptr_opt fn = nullable_view (funptr fn)

let ptr_opt t = nullable_view (Static.ptr t)

let string_opt = nullable_view string
