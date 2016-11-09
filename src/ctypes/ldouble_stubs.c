/*
 * Copyright (c) 2016 Andy Ray.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 */

#if !__USE_MINGW_ANSI_STDIO && (defined(__MINGW32__) || defined(__MINGW64__))
#define __USE_MINGW_ANSI_STDIO 1
#endif

#include <caml/mlvalues.h>
#include <caml/custom.h>
#include <caml/alloc.h>
#include <caml/intext.h>
#include <caml/fail.h>
#include <caml/hash.h>
#include <caml/memory.h>

#include <stdio.h>
#include <stdint.h>
#include <complex.h>

#include "ctypes_ldouble_stubs.h"

/*********************** long double *************************/

#define LDOUBLE_STORAGE_BYTES sizeof(long double)
#if (LDBL_MANT_DIG == 53)      // 64 bit - same as double
#define LDOUBLE_VALUE_BYTES 8
#elif (LDBL_MANT_DIG == 64)    // intel 80 bit extended 
#define LDOUBLE_VALUE_BYTES 10
#elif (LDBL_MANT_DIG == 106)   // __ibm128 (pair of doubles)
#define LDOUBLE_VALUE_BYTES 16
#elif (LDBL_MANT_DIG == 113)   // ieee __float128
#define LDOUBLE_VALUE_BYTES 16
#else
#define LDOUBLE_VALUE_BYTES LDOUBLE_STORAGE_BYTES
#endif

#define ldouble_custom_val(V) (*(long double *)(Data_custom_val(V)))

static int ldouble_cmp(long double u1, long double u2) {
  return (u1 == u2) ? 0 : (u1 < u2 ? -1 : 1);
}

static int ldouble_cmp_val(value v1, value v2)
{
  long double u1 = ldouble_custom_val(v1);
  long double u2 = ldouble_custom_val(v2);
  return ldouble_cmp(u1, u2);
}

static uint32_t ldouble_mix_hash(uint32_t hash, long double d) {
  union {
    long double d;
    uint32_t a[(LDOUBLE_STORAGE_BYTES+3)/4];
  } u;
  u.d = d;
 
  if (LDOUBLE_VALUE_BYTES == 16) {
    // ieee quad or __ibm128
#ifdef ARCH_BIG_ENDIAN
    hash = caml_hash_mix_uint32(hash, u.a[0]);
    hash = caml_hash_mix_uint32(hash, u.a[1]);
    hash = caml_hash_mix_uint32(hash, u.a[2]);
    hash = caml_hash_mix_uint32(hash, u.a[3]);
#else
    hash = caml_hash_mix_uint32(hash, u.a[1]);
    hash = caml_hash_mix_uint32(hash, u.a[0]);
    hash = caml_hash_mix_uint32(hash, u.a[3]);
    hash = caml_hash_mix_uint32(hash, u.a[2]);
#endif
  } else if (LDOUBLE_VALUE_BYTES == 10) {
    // intel extended 
    hash = caml_hash_mix_uint32(hash, u.a[0]);
    hash = caml_hash_mix_uint32(hash, u.a[1]);
    hash = caml_hash_mix_uint32(hash, u.a[2] & 0xFFFF);
  } else {
    // either LDOUBLE_VALUE_BYTES == 8, or we dont know what else to do.
    hash = caml_hash_mix_double(hash,  (double) d);
  }
  return hash;

}

static intnat ldouble_hash(value v) {
  return ldouble_mix_hash(0, ldouble_custom_val(v));
}

static struct custom_operations caml_ldouble_ops = {
  "ctypes:ldouble",
  custom_finalize_default,
  ldouble_cmp_val,
  ldouble_hash,
  NULL, //ldouble_serialize,
  NULL, //ldouble_deserialize,
  custom_compare_ext_default
};

value ctypes_copy_ldouble(long double u)
{
  value res = caml_alloc_custom(&caml_ldouble_ops, sizeof(long double), 0, 1);
  ldouble_custom_val(res) = u;
  return res;
}

long double ctypes_ldouble_val(value v) {
  return ldouble_custom_val(v);
}

/* of_double : float -> t */
value ctypes_ldouble_of_float(value a)
{
  return ctypes_copy_ldouble(Double_val(a));
}

/* to_double : t -> float */
value ctypes_ldouble_to_float(value a)
{
  return caml_copy_double(ldouble_custom_val(a));
}

value ctypes_ldouble_min(void) { return ctypes_copy_ldouble(-LDBL_MAX); }
value ctypes_ldouble_max(void) { return ctypes_copy_ldouble(LDBL_MAX); }
value ctypes_ldouble_epsilon(void) { return ctypes_copy_ldouble(LDBL_EPSILON); }
value ctypes_ldouble_nan(void) { return ctypes_copy_ldouble(nanl("char-sequence")); }
// XXX note; -(log 0) gives +ve inf (and vice versa).  Is this consistent? *)
value ctypes_ldouble_inf(void) { return ctypes_copy_ldouble(-log(0)); } 
value ctypes_ldouble_ninf(void) { return ctypes_copy_ldouble(log(0)); }

value ctypes_ldouble_size(void) {
  value r = caml_alloc_tuple(2);
  Field(r,0) = Val_int(LDOUBLE_STORAGE_BYTES);
  Field(r,1) = Val_int(LDOUBLE_VALUE_BYTES);
  return r;
}

/*********************** complex *************************/

#define ldouble_complex_custom_val(V) (*(long double complex*)(Data_custom_val(V)))

static int ldouble_complex_cmp_val(value v1, value v2)
{
  long double complex u1 = ldouble_custom_val(v1);
  long double complex u2 = ldouble_custom_val(v2);
  int cmp_real = ldouble_cmp(creall(u1), creall(u2));
  return cmp_real == 0 ? ldouble_cmp(cimagl(u1), cimagl(u2)) : cmp_real;
}

static intnat ldouble_complex_hash(value v) {
  long double complex c = ldouble_complex_custom_val(v);
  return ldouble_mix_hash(ldouble_mix_hash(0, creall(c)), cimagl(c));
}

static struct custom_operations caml_ldouble_complex_ops = {
  "ctypes:ldouble_complex",
  custom_finalize_default,
  ldouble_complex_cmp_val,
  ldouble_complex_hash,
  NULL, //ldouble_complex_serialize,
  NULL, //ldouble_complex_deserialize,
  custom_compare_ext_default
};

value ctypes_copy_ldouble_complex(long double complex u)
{
  value res = caml_alloc_custom(&caml_ldouble_complex_ops, sizeof(long double complex), 0, 1);
  ldouble_complex_custom_val(res) = u;
  return res;
}

long double complex ctypes_ldouble_complex_val(value v) {
  return ldouble_complex_custom_val(v);
}

/* make : t -> t -> complex */
value ctypes_ldouble_complex_make(value r, value i) {
  long double re = ldouble_custom_val(r);
  long double im = ldouble_custom_val(i);
  return ctypes_copy_ldouble_complex(re + (im * I));
}

/* real : complex -> t */
value ctypes_ldouble_complex_real(value v) {
  return ctypes_copy_ldouble(creall(ldouble_complex_custom_val(v)));
}

/* image : complex -> t */
value ctypes_ldouble_complex_imag(value v) {
  return ctypes_copy_ldouble(cimagl(ldouble_complex_custom_val(v)));
}


