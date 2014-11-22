/*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 */

#if !__USE_MINGW_ANSI_STDIO && (defined(__MINGW32__) || defined(__MINGW64__))
#define __USE_MINGW_ANSI_STDIO 1
#endif

#include <inttypes.h>
#include <stdio.h>
#include <assert.h>
#include <complex.h>
#include <string.h>
#include <stdbool.h>

#include <caml/memory.h>
#include <caml/alloc.h>

#include "type_info_stubs.h"
#include "unsigned_stubs.h"
#include "complex_stubs.h"
#include "raw_pointer.h"
#include "primitives.h"

#if __USE_MINGW_ANSI_STDIO && defined(__MINGW64__)
#define REAL_ARCH_INTNAT_PRINTF_FORMAT "ll"
#else
#define REAL_ARCH_INTNAT_PRINTF_FORMAT ARCH_INTNAT_PRINTF_FORMAT
#endif

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
   case Uchar: b = ctypes_copy_uint8(*(unsigned char *)buf); break;
   case Bool: b = Val_bool(*(bool *)buf); break;
   case Short: b = Val_int(*(short *)buf); break;
   case Int: b = Val_int(*(int *)buf); break;
   case Long: b = ctypes_copy_long(*(long *)buf); break;
   case Llong: b = ctypes_copy_llong(*(long long *)buf); break;
   case Ushort: b = ctypes_copy_ushort(*(unsigned short *)buf); break;
   case Uint: b = ctypes_copy_uint(*(unsigned int *)buf); break;
   case Ulong: b = ctypes_copy_ulong(*(unsigned long *)buf); break;
   case Ullong: b = ctypes_copy_ullong(*(unsigned long long *)buf); break;
   case Size_t: b = ctypes_copy_size_t(*(size_t *)buf); break;
   case Int8_t: b = Val_int(*(int8_t *)buf); break;
   case Int16_t: b = Val_int(*(int16_t *)buf); break;
   case Int32_t: b = caml_copy_int32(*(int32_t *)buf); break;
   case Int64_t: b = caml_copy_int64(*(int64_t *)buf); break;
   case Uint8_t: b = ctypes_copy_uint8(*(uint8_t *)buf); break;
   case Uint16_t: b = ctypes_copy_uint16(*(uint16_t *)buf); break;
   case Uint32_t: b = ctypes_copy_uint32(*(uint32_t *)buf); break;
   case Uint64_t: b = ctypes_copy_uint64(*(uint64_t *)buf); break;
   case Camlint: b = Val_int(*(intnat *)buf); break;
   case Nativeint: b = caml_copy_nativeint(*(intnat *)buf); break;
   case Float: b = caml_copy_double(*(float *)buf); break;
   case Double: b = caml_copy_double(*(double *)buf); break;
   case Complex32: b = ctypes_copy_float_complex(*(float complex *)buf); break;
   case Complex64: b = ctypes_copy_double_complex(*(double complex *)buf); break;
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
   case Uchar: *(unsigned char *)buf = Uint8_val(v); break;
   case Bool: *(bool *)buf = Bool_val(v); break;
   case Short: *(short *)buf = Int_val(v); break;
   case Int: *(int *)buf = Int_val(v); break;
   case Long: *(long *)buf = ctypes_long_val(v); break;
   case Llong: *(long long *)buf = ctypes_llong_val(v); break;
   case Ushort: *(unsigned short *)buf = ctypes_ushort_val(v); break;
   case Uint: *(unsigned int *)buf = ctypes_uint_val(v); break;
   case Ulong: *(unsigned long *)buf = ctypes_ulong_val(v); break;
   case Ullong: *(unsigned long long *)buf = ctypes_ullong_val(v); break;
   case Size_t: *(size_t *)buf = ctypes_size_t_val(v); break;
   case Int8_t: *(int8_t *)buf = Int_val(v); break;
   case Int16_t: *(int16_t *)buf = Int_val(v); break;
   case Int32_t: *(int32_t *)buf = Int32_val(v); break;
   case Int64_t: *(int64_t *)buf = Int64_val(v); break;
   case Uint8_t: *(uint8_t *)buf = Uint8_val(v); break;
   case Uint16_t: *(uint16_t *)buf = Uint16_val(v); break;
   case Uint32_t: *(uint32_t *)buf = Uint32_val(v); break;
   case Uint64_t: *(uint64_t *)buf = Uint64_val(v); break;
   case Camlint: *(intnat *)buf = Int_val(v); break;
   case Nativeint: *(intnat *)buf = Nativeint_val(v); break;
   case Float: *(float *)buf = Double_val(v); break;
   case Double: *(double *)buf = Double_val(v); break;
   case Complex32: *(float complex *)buf = ctypes_float_complex_val(v); break;
   case Complex64: *(double complex *)buf = ctypes_double_complex_val(v); break;
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
  case Uchar: len = snprintf(buf, sizeof buf, "%d", (unsigned char)Uint8_val(v)); break;
  case Bool: len = snprintf(buf, sizeof buf, "%s", Bool_val(v) ? "true" : "false"); break;
  case Short: len = snprintf(buf, sizeof buf, "%hd", (short)Int_val(v)); break;
  case Int: len = snprintf(buf, sizeof buf, "%d", Int_val(v)); break;
  case Long: len = snprintf(buf, sizeof buf, "%ld", (long)ctypes_long_val(v)); break;
  case Llong: len = snprintf(buf, sizeof buf, "%lld", (long long)ctypes_llong_val(v)); break;
  case Ushort: len = snprintf(buf, sizeof buf, "%hu", (unsigned short)ctypes_ushort_val(v)); break;
  case Uint: len = snprintf(buf, sizeof buf, "%u", (unsigned)ctypes_uint_val(v)); break;
  case Ulong: len = snprintf(buf, sizeof buf, "%lu", (unsigned long)ctypes_ulong_val(v)); break;
  case Ullong: len = snprintf(buf, sizeof buf, "%llu", (unsigned long long)ctypes_ullong_val(v)); break;
  case Size_t: len = snprintf(buf, sizeof buf, "%zu", (size_t)ctypes_size_t_val(v)); break;
  case Int8_t: len = snprintf(buf, sizeof buf, "%" PRId8, (int8_t)Int_val(v)); break;
  case Int16_t: len = snprintf(buf, sizeof buf, "%" PRId16, (int16_t)Int_val(v)); break;
  case Int32_t: len = snprintf(buf, sizeof buf, "%" PRId32, Int32_val(v)); break;
  case Int64_t: len = snprintf(buf, sizeof buf, "%" PRId64, (int64_t)Int64_val(v)); break;
  case Uint8_t: len = snprintf(buf, sizeof buf, "%" PRIu8, Uint8_val(v)); break;
  case Uint16_t: len = snprintf(buf, sizeof buf, "%" PRIu16, Uint16_val(v)); break;
  case Uint32_t: len = snprintf(buf, sizeof buf, "%" PRIu32, Uint32_val(v)); break;
  case Uint64_t: len = snprintf(buf, sizeof buf, "%" PRIu64, Uint64_val(v)); break;
  case Camlint: len = snprintf(buf, sizeof buf, "%" REAL_ARCH_INTNAT_PRINTF_FORMAT "d",
                         (intnat)Int_val(v)); break;
  case Nativeint: len = snprintf(buf, sizeof buf, "%" REAL_ARCH_INTNAT_PRINTF_FORMAT "d",
                           (intnat)Nativeint_val(v)); break;
  case Float: len = snprintf(buf, sizeof buf, "%.12g", Double_val(v)); break;
  case Double: len = snprintf(buf, sizeof buf, "%.12g", Double_val(v)); break;
  case Complex32: {
    float complex c = ctypes_float_complex_val(v);
    len = snprintf(buf, sizeof buf, "%.12g+%.12gi", crealf(c), cimagf(c));
    break;
  }
  case Complex64: {
    double complex c = ctypes_double_complex_val(v);
    len = snprintf(buf, sizeof buf, "%.12g+%.12gi", creal(c), cimag(c));
    break;
  }
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
