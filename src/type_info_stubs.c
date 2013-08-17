/*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 */

#include <inttypes.h>
#include <stdio.h>
#include <assert.h>
#include <complex.h>

#include <caml/memory.h>
#include <caml/alloc.h>

#include "unsigned_stubs.h"
#include "raw_pointer.h"
#include "primitives.h"

static value allocate_complex_value(double r, double i)
{
  value v = caml_alloc(2 * sizeof(double), Double_array_tag);
  Store_double_field(v, 0, r);
  Store_double_field(v, 1, i);
  return v;
}

/* Read a C value from a block of memory */
/* read : 'a prim -> offset:int -> raw_pointer -> 'a */
value ctypes_read(value prim_, value offset_, value buffer_)
{
  CAMLparam3(prim_, offset_, buffer_);
  CAMLlocal1(b);
  int offset = Int_val(offset_);
  void *buf = (char *)CTYPES_TO_PTR(buffer_) + offset;
  switch (Int_val(prim_))
  {
   case Char: b = Val_int(*(char *)buf); break;
   case Schar: b = Val_int(*(signed char *)buf); break;
   case Uchar: b = ctypes_copy_uint8(*(unsigned char *)buf); break;
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
   case Uint64_t: b = ctypes_copy_uint64(*(uint64 *)buf); break;
   case Camlint: b = Val_int(*(intnat *)buf); break;
   case Nativeint: b = caml_copy_nativeint(*(intnat *)buf); break;
   case Float: b = caml_copy_double(*(float *)buf); break;
   case Double: b = caml_copy_double(*(double *)buf); break;
  case Complex32: {
    float complex c = *(float complex *)buf;
    b = allocate_complex_value(crealf(c), cimagf(c));
    break;
  }
  case Complex64: {
    double complex c = *(double complex *)buf;
    b = allocate_complex_value(creal(c), cimag(c));
    break;
  }
  default:
    assert(0);
  }
  CAMLreturn(b);
}

/* Read a C value from a block of memory */
/* write : 'a prim -> offset:int -> 'a -> raw_pointer -> unit */
value ctypes_write(value prim_, value offset_, value v, value buffer_)
{
  CAMLparam4(prim_, offset_, v, buffer_);
  int offset = Int_val(offset_);
  void *buf = (char *)CTYPES_TO_PTR(buffer_) + offset;
  switch (Int_val(prim_))
  {
   case Char: *(char *)buf = Int_val(v); break;
   case Schar: *(signed char *)buf = Int_val(v); break;
   case Uchar: *(unsigned char *)buf = Uint8_val(v); break;
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
   case Uint64_t: *(uint64 *)buf = Uint64_val(v); break;
   case Camlint: *(intnat *)buf = Int_val(v); break;
   case Nativeint: *(intnat *)buf = Nativeint_val(v); break;
   case Float: *(float *)buf = Double_val(v); break;
   case Double: *(double *)buf = Double_val(v); break;
   case Complex32:
     *(float complex *)buf = Double_field(v, 0) + Double_field(v, 1) * I;
     break;
   case Complex64:
     *(double complex *)buf = Double_field(v, 0) + Double_field(v, 1) * I;
     break;
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
  char buf[64];
  char *format = NULL;
  switch (Int_val(prim_))
  {
  case Char: snprintf(buf, sizeof buf, "'%c'", Int_val(v)); break;
  case Schar: snprintf(buf, sizeof buf, "%d", Int_val(v)); break;
  case Uchar: snprintf(buf, sizeof buf, "%d", (unsigned char)Uint8_val(v)); break;
  case Short: snprintf(buf, sizeof buf, "%hd", Int_val(v)); break;
  case Int: snprintf(buf, sizeof buf, "%d", Int_val(v)); break;
  case Long: snprintf(buf, sizeof buf, "%ld", (long)ctypes_long_val(v)); break;
  case Llong: snprintf(buf, sizeof buf, "%lld", (long long)ctypes_llong_val(v)); break;
  case Ushort: snprintf(buf, sizeof buf, "%hu", (unsigned short)ctypes_ushort_val(v)); break;
  case Uint: snprintf(buf, sizeof buf, "%u", (unsigned)ctypes_uint_val(v)); break;
  case Ulong: snprintf(buf, sizeof buf, "%lu", (unsigned long)ctypes_ulong_val(v)); break;
  case Ullong: snprintf(buf, sizeof buf, "%llu", (unsigned long long)ctypes_ullong_val(v)); break;
  case Size_t: snprintf(buf, sizeof buf, "%zu", (size_t)ctypes_size_t_val(v)); break;
  case Int8_t: snprintf(buf, sizeof buf, "%" PRId8, Int_val(v)); break;
  case Int16_t: snprintf(buf, sizeof buf, "%" PRId16, Int_val(v)); break;
  case Int32_t: snprintf(buf, sizeof buf, "%" PRId32, Int32_val(v)); break;
  case Int64_t: snprintf(buf, sizeof buf, "%" PRId64, Int64_val(v)); break;
  case Uint8_t: snprintf(buf, sizeof buf, "%" PRIu8, Uint8_val(v)); break;
  case Uint16_t: snprintf(buf, sizeof buf, "%" PRIu16, Uint16_val(v)); break;
  case Uint32_t: snprintf(buf, sizeof buf, "%" PRIu32, Uint32_val(v)); break;
  case Uint64_t: snprintf(buf, sizeof buf, "%" PRIu64, Uint64_val(v)); break;
  case Camlint: snprintf(buf, sizeof buf, "%" ARCH_INTNAT_PRINTF_FORMAT "d",
                         (intnat)Int_val(v)); break;
  case Nativeint: snprintf(buf, sizeof buf, "%" ARCH_INTNAT_PRINTF_FORMAT "d",
                           (intnat)Nativeint_val(v)); break;
  case Float: snprintf(buf, sizeof buf, "%.12g", Double_val(v)); break;
  case Double: snprintf(buf, sizeof buf, "%.12g", Double_val(v)); break;
  case Complex32: 
    snprintf(buf, sizeof buf, "%.12g+%.12gi", Double_field(v, 0),
             Double_field(v, 1));
    break;
  case Complex64:
    snprintf(buf, sizeof buf, "%.12g+%.12gi", Double_field(v, 0),
             Double_field(v, 1));
    break;
  default:
    assert(0);
  }
  CAMLreturn (caml_copy_string(buf));
}

/* read_pointer : offset:int -> raw_pointer -> raw_pointer */
value ctypes_read_pointer(value offset_, value src_)
{
  CAMLparam2(offset_, src_);
  void *src = (char *)CTYPES_TO_PTR(src_) + Int_val(offset_);
  CAMLreturn(CTYPES_FROM_PTR(*(void **)src));
}

/* write_pointer : offset:int -> raw_pointer -> dst:raw_pointer -> unit */
value ctypes_write_pointer(value offset_, value p_, value dst_)
{
  CAMLparam3(offset_, p_, dst_);
  int offset = Int_val(offset_);
  void *dst = (char *)CTYPES_TO_PTR(dst_) + Int_val(offset_);
  *(void **)dst = CTYPES_TO_PTR(p_);
  CAMLreturn(Val_unit);
}

/* string_of_pointer : raw_pointer -> string */
value ctypes_string_of_pointer(value p_)
{
  char buf[32];
  CAMLparam1(p_);
  snprintf(buf, sizeof buf, "%p", CTYPES_TO_PTR(p_));
  CAMLreturn (caml_copy_string(buf));
}
