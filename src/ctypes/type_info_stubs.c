/*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 */

#include <inttypes.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>

#include <caml/memory.h>
#include <caml/alloc.h>

#include "type_info_stubs.h"
#include "raw_pointer.h"
#include "primitives.h"

/* Read a C value from a block of memory */
/* read : 'a prim -> fat_pointer -> 'a */
value ctypes_read(value prim_, value buffer_)
{
  CAMLparam2(prim_, buffer_);
  CAMLlocal1(b);
  void *buf = CTYPES_ADDR_OF_FATPTR(buffer_);
  switch (Int_val(prim_))
  {
   case Char: b = Val_int(*(char *)buf); break;
   case Schar: b = Val_int(*(signed char *)buf); break;
   case Short: b = Val_int(*(short *)buf); break;
   case Int: b = Val_int(*(int *)buf); break;
   case Int8_t: b = Val_int(*(int8_t *)buf); break;
   case Int16_t: b = Val_int(*(int16_t *)buf); break;
   case Int32_t: b = caml_copy_int32(*(int32_t *)buf); break;
   case Int64_t: b = caml_copy_int64(*(int64_t *)buf); break;
   case Camlint: b = Val_int(*(intnat *)buf); break;
   case Nativeint: b = caml_copy_nativeint(*(intnat *)buf); break;
   case Float: b = caml_copy_double(*(float *)buf); break;
   case Double: b = caml_copy_double(*(double *)buf); break;
   default:
    assert(0);
  }
  CAMLreturn(b);
}

/* Read a C value from a block of memory */
/* write : 'a prim -> 'a -> fat_pointer -> unit */
value ctypes_write(value prim_, value v, value buffer_)
{
  CAMLparam3(prim_, v, buffer_);
  void *buf = CTYPES_ADDR_OF_FATPTR(buffer_);
  switch (Int_val(prim_))
  {
   case Char: *(char *)buf = Int_val(v); break;
   case Schar: *(signed char *)buf = Int_val(v); break;
   case Short: *(short *)buf = Int_val(v); break;
   case Int: *(int *)buf = Int_val(v); break;
   case Int8_t: *(int8_t *)buf = Int_val(v); break;
   case Int16_t: *(int16_t *)buf = Int_val(v); break;
   case Int32_t: *(int32_t *)buf = Int32_val(v); break;
   case Int64_t: *(int64_t *)buf = Int64_val(v); break;
   case Camlint: *(intnat *)buf = Int_val(v); break;
   case Nativeint: *(intnat *)buf = Nativeint_val(v); break;
   case Float: *(float *)buf = Double_val(v); break;
   case Double: *(double *)buf = Double_val(v); break;
   default:
    assert(0);
  }
  CAMLreturn(Val_unit);
}

/* Format a C value */
/* string_of_prim : 'a prim -> 'a -> string */
value ctypes_string_of_prim(value prim_, value v)
{
  CAMLparam2(prim_, v);
  CAMLlocal1(s);
  char buf[64];
  int len = 0;
  switch (Int_val(prim_))
  {
  case Char: len = snprintf(buf, sizeof buf, "'%c'", Int_val(v)); break;
  case Schar: len = snprintf(buf, sizeof buf, "%d", Int_val(v)); break;
  case Short: len = snprintf(buf, sizeof buf, "%hd", (short)Int_val(v)); break;
  case Int: len = snprintf(buf, sizeof buf, "%d", Int_val(v)); break;
  case Int8_t: len = snprintf(buf, sizeof buf, "%" PRId8, (int8_t)Int_val(v)); break;
  case Int16_t: len = snprintf(buf, sizeof buf, "%" PRId16, (int16_t)Int_val(v)); break;
  case Int32_t: len = snprintf(buf, sizeof buf, "%" PRId32, Int32_val(v)); break;
  case Int64_t: len = snprintf(buf, sizeof buf, "%" PRId64, Int64_val(v)); break;
  case Camlint: len = snprintf(buf, sizeof buf, "%" ARCH_INTNAT_PRINTF_FORMAT "d",
                         (intnat)Int_val(v)); break;
  case Nativeint: len = snprintf(buf, sizeof buf, "%" ARCH_INTNAT_PRINTF_FORMAT "d",
                           (intnat)Nativeint_val(v)); break;
  case Float: len = snprintf(buf, sizeof buf, "%.12g", Double_val(v)); break;
  case Double: len = snprintf(buf, sizeof buf, "%.12g", Double_val(v)); break;
  default:
    assert(0);
  }
  s = caml_alloc_string(len);
  memcpy(String_val(s), buf, len);
  CAMLreturn (s);
}

/* read_pointer : fat_pointer -> raw_pointer */
value ctypes_read_pointer(value src_)
{
  CAMLparam1(src_);
  void *src = CTYPES_ADDR_OF_FATPTR(src_);
  CAMLreturn(CTYPES_FROM_PTR(*(void **)src));
}

/* write_pointer : fat_pointer -> dst:fat_pointer -> unit */
value ctypes_write_pointer(value p_, value dst_)
{
  CAMLparam2(p_, dst_);
  void *dst = CTYPES_ADDR_OF_FATPTR(dst_);
  *(void **)dst = CTYPES_ADDR_OF_FATPTR(p_);
  CAMLreturn(Val_unit);
}

/* string_of_pointer : fat_pointer -> string */
value ctypes_string_of_pointer(value p_)
{
  char buf[32];
  CAMLparam1(p_);
  snprintf(buf, sizeof buf, "%p", CTYPES_ADDR_OF_FATPTR(p_));
  CAMLreturn (caml_copy_string(buf));
}
