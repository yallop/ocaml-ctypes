/*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 */

#include <string.h>

#include <caml/memory.h>

#include "managed_buffer_stubs.h"
#include "type_info_stubs.h"
#include "raw_pointer.h"

/* memcpy : dest:raw_pointer -> dest_offset:int ->
            src:raw_pointer -> src_offset:int ->
            size:int -> unit */
value ctypes_memcpy(value dst, value dst_offset,
                    value src, value src_offset, value size)
{
  CAMLparam5(dst, dst_offset, src, src_offset, size);
  memcpy((char *)CTYPES_TO_PTR(dst) + Int_val(dst_offset),
         (char *)CTYPES_TO_PTR(src) + Int_val(src_offset),
         Int_val(size));
  CAMLreturn(Val_unit);
}


/* string_of_cstring : raw_ptr -> int -> string */
value ctypes_string_of_cstring(value p, value offset)
{
  return caml_copy_string(((char *)CTYPES_TO_PTR(p))
                          + Int_val(offset));
}


/* string_of_array : raw_ptr -> off:int -> len:int -> string */
value ctypes_string_of_array(value p, value offset, value vlen)
{
  CAMLparam3(p, offset, vlen);
  CAMLlocal1(dst);
  int len = Int_val(vlen);
  if (len < 0)
    caml_invalid_argument("ctypes_string_of_array");
  dst = caml_alloc_string(len);
  memcpy(String_val(dst),
         (char *)CTYPES_TO_PTR(p) + Int_val(offset),
         len);
  CAMLreturn(dst);
}


/* cstring_of_string : string -> managed_buffer */
value ctypes_cstring_of_string(value s)
{
  CAMLparam1(s);
  CAMLlocal1(buffer);
  int len = caml_string_length(s);
  buffer = ctypes_allocate(Val_int(len + 1));
  char *dst = CTYPES_TO_PTR(ctypes_block_address(buffer));
  char *ss = String_val(s);
  memcpy(dst, ss, len);
  dst[len] = '\0';
  CAMLreturn(buffer);
}
