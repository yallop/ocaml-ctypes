/*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 */

#include <string.h>

#include <caml/memory.h>
#include <caml/fail.h>

#include "managed_buffer_stubs.h"
#include "type_info_stubs.h"
#include "raw_pointer.h"

/* memcpy : dst:raw_pointer -> src:raw_pointer -> size:int -> unit */
value ctypes_memcpy(value dst, value src, value size)
{
  CAMLparam3(dst, src, size);
  memcpy(CTYPES_TO_PTR(dst), CTYPES_TO_PTR(src), Int_val(size));
  CAMLreturn(Val_unit);
}


/* string_of_cstring : raw_ptr -> int -> string */
value ctypes_string_of_cstring(value p)
{
  return caml_copy_string(CTYPES_TO_PTR(p));
}


/* string_of_array : raw_ptr -> len:int -> string */
value ctypes_string_of_array(value p, value vlen)
{
  CAMLparam2(p, vlen);
  CAMLlocal1(dst);
  int len = Int_val(vlen);
  if (len < 0)
    caml_invalid_argument("ctypes_string_of_array");
  dst = caml_alloc_string(len);
  memcpy(String_val(dst), CTYPES_TO_PTR(p), len);
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
