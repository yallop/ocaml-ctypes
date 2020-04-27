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
#include <string.h>
#include <stdbool.h>

#include <caml/memory.h>
#include <caml/alloc.h>

#include "ocaml_integers.h"
#include "ctypes_type_info_stubs.h"
#include "ctypes_complex_compatibility.h"
#include "ctypes_complex_stubs.h"
#include "ctypes_ldouble_stubs.h"
#include "ctypes_raw_pointer.h"
#include "ctypes_primitives.h"

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
   case Ctypes_Char: b = Val_int(*(unsigned char*)buf); break;
   case Ctypes_Schar: b = Val_int(*(signed char *)buf); break;
   case Ctypes_Uchar: b = Integers_val_uint8(*(unsigned char *)buf); break;
   case Ctypes_Bool: {
     bool s;
     memcpy(&s,buf,sizeof(s));
     b = Val_bool(s);
     break;
   }
   case Ctypes_Short: {
     short s;
     memcpy(&s, buf, sizeof(s));
     b = Val_int(s);
     break;
   }
   case Ctypes_Int: {
     int s;
     memcpy(&s, buf, sizeof(s));
     b = Val_int(s);
     break;
   }
   case Ctypes_Long: {
     long s;
     memcpy(&s, buf, sizeof(s));
     b = ctypes_copy_long(s);
     break;
   }
   case Ctypes_Llong: {
     long long s;
     memcpy(&s, buf, sizeof(s));
     b = ctypes_copy_llong(s);
     break;
   }
   case Ctypes_Ushort: {
     unsigned short s;
     memcpy(&s, buf, sizeof(s));
     b = ctypes_copy_ushort(s);
     break;
   }
   case Ctypes_Sint: {
     int s;
     memcpy(&s, buf, sizeof(s));
     b = ctypes_copy_sint(s);
     break;
   }
   case Ctypes_Uint: {
     unsigned int s;
     memcpy(&s, buf, sizeof(s));
     b = ctypes_copy_uint(s);
     break;
   }
   case Ctypes_Ulong: {
     unsigned long s;
     memcpy(&s, buf, sizeof(s));
     b = ctypes_copy_ulong(s);
     break;
   }
   case Ctypes_Ullong: {
     unsigned long long s;
     memcpy(&s, buf, sizeof(s));
     b = ctypes_copy_ullong(s);
     break;
   }
   case Ctypes_Size_t: {
     size_t s;
     memcpy(&s, buf, sizeof(s));
     b = ctypes_copy_size_t(s);
     break;
   }
   case Ctypes_Int8_t: b = Val_int(*(int8_t *)buf); break;
   case Ctypes_Int16_t: {
     int16_t s;
     memcpy(&s, buf, sizeof(s));
     b = Val_int(s);
     break;
   }
   case Ctypes_Int32_t: {
     int32_t s;
     memcpy(&s, buf, sizeof(s));
     b = caml_copy_int32(s);
     break;
   }
   case Ctypes_Int64_t: {
     int64_t s;
     memcpy(&s, buf, sizeof(s));
     b = caml_copy_int64(s);
     break;
   }
   case Ctypes_Uint8_t: b = Integers_val_uint8(*(uint8_t *)buf); break;
   case Ctypes_Uint16_t: {
     uint16_t s;
     memcpy(&s, buf, sizeof(s));
     b = Integers_val_uint16(s);
     break;
   }
   case Ctypes_Uint32_t: {
     uint32_t s;
     memcpy(&s, buf, sizeof(s));
     b = integers_copy_uint32(s);
     break;
   }
   case Ctypes_Uint64_t: {
     uint64_t s;
     memcpy(&s, buf, sizeof(s));
     b = integers_copy_uint64(s);
     break;
   }
   case Ctypes_Camlint: {
     intnat s;
     memcpy(&s, buf, sizeof(s));
     b = Val_long(s);
     break;
   }
   case Ctypes_Nativeint: {
     intnat s;
     memcpy(&s, buf, sizeof(s));
     b = caml_copy_nativeint(s);
     break;
   }
   case Ctypes_Float: {
     float s;
     memcpy(&s, buf, sizeof(s));
     b = caml_copy_double(s);
     break;
   }
   case Ctypes_Double: {
     double s;
     memcpy(&s, buf, sizeof(s));
     b = caml_copy_double(s);
     break;
   }
   case Ctypes_LDouble: {
     long double s;
     memcpy(&s, buf, sizeof(s));
     b = ctypes_copy_ldouble(s);
     break;
   }
   case Ctypes_Complex32: {
     float _Complex s;
     memcpy(&s, buf, sizeof(s));
     b = ctypes_copy_float_complex(s);
     break;
   }
   case Ctypes_Complex64: {
     double _Complex s;
     memcpy(&s, buf, sizeof(s));
     b = ctypes_copy_double_complex(s);
     break;
   }
   case Ctypes_Complexld: {
     long double _Complex s;
     memcpy(&s, buf, sizeof(s));
     b = ctypes_copy_ldouble_complex(s);
     break;
   }
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
   case Ctypes_Char: *(unsigned char *)buf = Int_val(v); break;
   case Ctypes_Schar: *(signed char *)buf = Int_val(v); break;
   case Ctypes_Uchar: *(unsigned char *)buf = Uint8_val(v); break;
   case Ctypes_Bool: {
     bool s = Bool_val(v);
     memcpy(buf, &s, sizeof(s));
     break;
   }
   case Ctypes_Short: {
     short s = Int_val(v);
     memcpy(buf, &s, sizeof(s));
     break;
   }
   case Ctypes_Int: {
     int s = Int_val(v);
     memcpy(buf, &s, sizeof(s));
     break;
   }
   case Ctypes_Long: {
     long s = ctypes_long_val(v);
     memcpy(buf, &s, sizeof(s));
     break;
   }
   case Ctypes_Llong: {
     long long s = ctypes_llong_val(v);
     memcpy(buf, &s, sizeof(s));
     break;
   }
   case Ctypes_Ushort: {
     unsigned short s = ctypes_ushort_val(v);
     memcpy(buf, &s, sizeof(s));
     break;
   }
   case Ctypes_Sint: {
     int s = ctypes_sint_val(v);
     memcpy(buf, &s, sizeof(s));
     break;
   }
   case Ctypes_Uint: {
     unsigned int s = ctypes_uint_val(v);
     memcpy(buf, &s, sizeof(s));
     break;
   }
   case Ctypes_Ulong: {
     unsigned long s = ctypes_ulong_val(v);
     memcpy(buf, &s, sizeof(s));
     break;
   }
   case Ctypes_Ullong: {
     unsigned long long s = ctypes_ullong_val(v);
     memcpy(buf, &s, sizeof(s));
     break;
   }
   case Ctypes_Size_t: {
     size_t s = ctypes_size_t_val(v);
     memcpy(buf, &s, sizeof(s));
     break;
   }
   case Ctypes_Int8_t: *(int8_t *)buf = Int_val(v); break;
   case Ctypes_Int16_t: {
     int16_t s = Int_val(v);
     memcpy(buf, &s, sizeof(s));
     break;
   }
   case Ctypes_Int32_t: {
     int32_t s = Int32_val(v);
     memcpy(buf, &s, sizeof(s));
     break;
   }
   case Ctypes_Int64_t: {
     int64_t s = Int64_val(v);
     memcpy(buf, &s, sizeof(s));
     break;
   }
   case Ctypes_Uint8_t: *(uint8_t *)buf = Uint8_val(v); break;
   case Ctypes_Uint16_t: {
     uint16_t s = Uint16_val(v);
     memcpy(buf, &s, sizeof(s));
     break;
   }
   case Ctypes_Uint32_t: {
     uint32_t s = Uint32_val(v);
     memcpy(buf, &s, sizeof(s));
     break;
   }
   case Ctypes_Uint64_t: {
     uint64_t s = Uint64_val(v);
     memcpy(buf, &s, sizeof(s));
     break;
   }
   case Ctypes_Camlint: {
     intnat s = Long_val(v);
     memcpy(buf, &s, sizeof(s));
     break;
   }
   case Ctypes_Nativeint: {
     intnat s = Nativeint_val(v);
     memcpy(buf, &s, sizeof(s));
     break;
   }
   case Ctypes_Float: {
     float s = Double_val(v);
     memcpy(buf, &s, sizeof(s));
     break;
   }
   case Ctypes_Double: {
     double s = Double_val(v);
     memcpy(buf, &s, sizeof(s));
     break;
   }
   case Ctypes_LDouble: {
     long double s = ctypes_ldouble_val(v);
     memcpy(buf, &s, sizeof(s));
     break;
   }
   case Ctypes_Complex32: {
     float _Complex s = ctypes_float_complex_val(v);
     memcpy(buf, &s, sizeof(s));
     break;
   }
   case Ctypes_Complex64: {
     double _Complex s = ctypes_double_complex_val(v);
     memcpy(buf, &s, sizeof(s));
     break;
   }
   case Ctypes_Complexld: {
     long double _Complex s = ctypes_ldouble_complex_val(v);
     memcpy(buf, &s, sizeof(s));
     break;
   }
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
  case Ctypes_Char: len = snprintf(buf, sizeof buf, "'%c'", Int_val(v)); break;
  case Ctypes_Schar: len = snprintf(buf, sizeof buf, "%d", Int_val(v)); break;
  case Ctypes_Uchar: len = snprintf(buf, sizeof buf, "%d", (unsigned char)Uint8_val(v)); break;
  case Ctypes_Bool: len = snprintf(buf, sizeof buf, "%s", Bool_val(v) ? "true" : "false"); break;
  case Ctypes_Short: len = snprintf(buf, sizeof buf, "%hd", (short)Int_val(v)); break;
  case Ctypes_Int: len = snprintf(buf, sizeof buf, "%d", Int_val(v)); break;
  case Ctypes_Long: len = snprintf(buf, sizeof buf, "%ld", (long)ctypes_long_val(v)); break;
  case Ctypes_Llong: len = snprintf(buf, sizeof buf, "%lld", (long long)ctypes_llong_val(v)); break;
  case Ctypes_Ushort: len = snprintf(buf, sizeof buf, "%hu", (unsigned short)ctypes_ushort_val(v)); break;
  case Ctypes_Sint: len = snprintf(buf, sizeof buf, "%d", ctypes_sint_val(v)); break;
  case Ctypes_Uint: len = snprintf(buf, sizeof buf, "%u", (unsigned)ctypes_uint_val(v)); break;
  case Ctypes_Ulong: len = snprintf(buf, sizeof buf, "%lu", (unsigned long)ctypes_ulong_val(v)); break;
  case Ctypes_Ullong: len = snprintf(buf, sizeof buf, "%llu", (unsigned long long)ctypes_ullong_val(v)); break;
  case Ctypes_Size_t: len = snprintf(buf, sizeof buf, "%zu", (size_t)ctypes_size_t_val(v)); break;
  case Ctypes_Int8_t: len = snprintf(buf, sizeof buf, "%" PRId8, (int8_t)Int_val(v)); break;
  case Ctypes_Int16_t: len = snprintf(buf, sizeof buf, "%" PRId16, (int16_t)Int_val(v)); break;
  case Ctypes_Int32_t: len = snprintf(buf, sizeof buf, "%" PRId32, Int32_val(v)); break;
  case Ctypes_Int64_t: len = snprintf(buf, sizeof buf, "%" PRId64, (int64_t)Int64_val(v)); break;
  case Ctypes_Uint8_t: len = snprintf(buf, sizeof buf, "%" PRIu8, Uint8_val(v)); break;
  case Ctypes_Uint16_t: len = snprintf(buf, sizeof buf, "%" PRIu16, Uint16_val(v)); break;
  case Ctypes_Uint32_t: len = snprintf(buf, sizeof buf, "%" PRIu32, Uint32_val(v)); break;
  case Ctypes_Uint64_t: len = snprintf(buf, sizeof buf, "%" PRIu64, Uint64_val(v)); break;
  case Ctypes_Camlint: len = snprintf(buf, sizeof buf, "%" REAL_ARCH_INTNAT_PRINTF_FORMAT "d",
                         (intnat)Long_val(v)); break;
  case Ctypes_Nativeint: len = snprintf(buf, sizeof buf, "%" REAL_ARCH_INTNAT_PRINTF_FORMAT "d",
                           (intnat)Nativeint_val(v)); break;
  case Ctypes_Float: len = snprintf(buf, sizeof buf, "%.12g", Double_val(v)); break;
  case Ctypes_Double: len = snprintf(buf, sizeof buf, "%.12g", Double_val(v)); break;
  case Ctypes_LDouble: len = snprintf(buf, sizeof buf, "%.12Lg", ctypes_ldouble_val(v)); break;
  case Ctypes_Complex32: {
    float _Complex c = ctypes_float_complex_val(v);
    len = snprintf(buf, sizeof buf, "%.12g+%.12gi", ctypes_compat_crealf(c), ctypes_compat_cimagf(c));
    break;
  }
  case Ctypes_Complex64: {
    double _Complex c = ctypes_double_complex_val(v);
    len = snprintf(buf, sizeof buf, "%.12g+%.12gi", ctypes_compat_creal(c), ctypes_compat_cimag(c));
    break;
  }
  case Ctypes_Complexld: {
    long double _Complex c = ctypes_ldouble_complex_val(v);
    len = snprintf(buf, sizeof buf, "%.12Lg+%.12Lgi", ctypes_compat_creall(c), ctypes_compat_cimagl(c));
    break;
  }
  default:
    assert(0);
  }
  s = caml_alloc_string(len);
  memcpy((char *)String_val(s), buf, len);
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
