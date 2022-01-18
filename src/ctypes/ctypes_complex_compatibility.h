/*
 * Copyright (c) 2018 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 */

#ifndef CTYPES_COMPLEX_COMPATIBILITY_H
#define CTYPES_COMPLEX_COMPATIBILITY_H

#include "ctypes_complex_types.h"
#include <caml/fail.h>

/* "Each complex type has the same representation and alignment
    requirements as an array type containing exactly two elements of
    the corresponding real type; the first element is equal to the real
    part, and the second element to the imaginary part, of the complex
    number."
                                                       - C99 6.2.5 (13)
*/
union ctypes_complex_long_double_union {
  ctypes_complex_long_double z;
  long double parts[2];
};

union ctypes_complex_double_union {
  ctypes_complex_double z;
  double parts[2];
};

union ctypes_complex_float_union {
  ctypes_complex_float z;
  float parts[2];
};

#if defined(__ANDROID__)
#define CTYPES_USE_STRUCT_BUILDER 1

#include <math.h>

static inline long double ctypes_compat_creall(ctypes_complex_long_double z)
{ union ctypes_complex_long_double_union u; u.z = z; return u.parts[0]; }

static inline long double ctypes_compat_cimagl(ctypes_complex_long_double z)
{ union ctypes_complex_long_double_union u; u.z = z; return u.parts[1]; }

static inline ctypes_complex_long_double ctypes_compat_conjl(ctypes_complex_long_double z)
{ union ctypes_complex_long_double_union u; u.z = z; u.parts[1] = -u.parts[1]; return u.z; }

static inline long double ctypes_compat_cargl(ctypes_complex_long_double z)
{ return atan2(ctypes_compat_cimagl(z), ctypes_compat_creall(z)); }

static inline double ctypes_compat_creal(ctypes_complex_double z)
{ union ctypes_complex_double_union u; u.z = z; return u.parts[0]; }

static inline double ctypes_compat_cimag(ctypes_complex_double z)
{ union ctypes_complex_double_union u; u.z = z; return u.parts[1]; }

static inline ctypes_complex_double ctypes_compat_conj(ctypes_complex_double z)
{ union ctypes_complex_double_union u; u.z = z; u.parts[1] = -u.parts[1]; return u.z; }

static inline float ctypes_compat_crealf(ctypes_complex_float z)
{ union ctypes_complex_float_union u; u.z = z; return u.parts[0]; }

static inline float ctypes_compat_cimagf(ctypes_complex_float z)
{ union ctypes_complex_float_union u; u.z = z; return u.parts[1]; }

static inline ctypes_complex_float ctypes_compat_conjf(ctypes_complex_float z)
{ union ctypes_complex_float_union u; u.z = z; u.parts[1] = -u.parts[1]; return u.z; }

/* Android: As of API level 24, these functions do not exist. */

static inline ctypes_complex_long_double ctypes_compat_csqrtl(ctypes_complex_long_double z)
{ caml_failwith("ctypes: csqrtl does not exist on current platform"); }

static inline ctypes_complex_long_double ctypes_compat_cexpl(ctypes_complex_long_double z)
{ caml_failwith("ctypes: cexpl does not exist on current platform"); }

static inline ctypes_complex_long_double ctypes_compat_clogl(ctypes_complex_long_double z)
{ caml_failwith("ctypes: clogl does not exist on current platform"); }

static inline ctypes_complex_long_double ctypes_compat_cpowl(ctypes_complex_long_double x, ctypes_complex_long_double z)
{ caml_failwith("ctypes: cpowl does not exist on current platform"); }


#else

#include <complex.h>

static inline long double ctypes_compat_creall(ctypes_complex_long_double z)
{ return creall(z); }
static inline long double ctypes_compat_cimagl(ctypes_complex_long_double z)
{ return cimagl(z); }
static inline ctypes_complex_long_double ctypes_compat_conjl(ctypes_complex_long_double z)
{ return conjl(z); }

#if defined(__FreeBSD__)
static inline ctypes_complex_long_double ctypes_compat_cexpl(ctypes_complex_long_double z)
{ caml_failwith("ctypes: cexpl does not exist on current platform"); }
#else
static inline ctypes_complex_long_double ctypes_compat_cexpl(ctypes_complex_long_double z)
{ return cexpl(z); }
#endif
static inline ctypes_complex_long_double ctypes_compat_clogl(ctypes_complex_long_double z)
{ return clogl(z); }
static inline ctypes_complex_long_double ctypes_compat_cpowl(ctypes_complex_long_double x, ctypes_complex_long_double z)
{ return cpowl(x, z); }

static inline ctypes_complex_long_double ctypes_compat_csqrtl(ctypes_complex_long_double z)
{ return csqrtl(z); }
static inline long double ctypes_compat_cargl(ctypes_complex_long_double z)
{ return cargl(z); }

static inline double ctypes_compat_creal(ctypes_complex_double z)
{ return creal(z); }
static inline double ctypes_compat_cimag(ctypes_complex_double z)
{ return cimag(z); }
static inline ctypes_complex_double ctypes_compat_conj(ctypes_complex_double z)
{ return conj(z); }

static inline float ctypes_compat_crealf(ctypes_complex_float z)
{ return crealf(z); }
static inline float ctypes_compat_cimagf(ctypes_complex_float z)
{ return cimagf(z); }
static inline ctypes_complex_float ctypes_compat_conjf(ctypes_complex_float z)
{ return conjf(z); }

#if !defined(CMPLXF) || !defined(CMPLX) || !defined(CMPLXL)
#define CTYPES_USE_STRUCT_BUILDER 1
#else
static inline ctypes_complex_double ctypes_compat_make_complex(double re, double im)
{ return (CMPLX(re,im)); }
static inline ctypes_complex_long_double ctypes_compat_make_complexl(long double re, long double im)
{ return (CMPLXL(re,im)); }
static inline ctypes_complex_float ctypes_compat_make_complexf(float re, float im)
{ return (CMPLXF(re,im)); }
#endif

#endif

#ifdef CTYPES_USE_STRUCT_BUILDER
static inline ctypes_complex_double ctypes_compat_make_complex(double re, double im)
{ union ctypes_complex_double_union u; u.parts[0] = re; u.parts[1] = im; return u.z; }
static inline ctypes_complex_float ctypes_compat_make_complexf(float re, float im)
{ union ctypes_complex_float_union u; u.parts[0] = re; u.parts[1] = im; return u.z; }
static inline ctypes_complex_long_double ctypes_compat_make_complexl(long double re, long double im)
{ union ctypes_complex_long_double_union u; u.parts[0] = re; u.parts[1] = im; return u.z; }
#undef CTYPES_USE_STRUCT_BUILDER
#endif

/* Primitive arithmetic */
#ifdef _MSC_VER
# define CCC_PRIM_COMPONENT_OP(OPNAME, TYP, UNION, OP)                                 \
  static inline TYP ctypes_compat_ ## OPNAME(TYP a, TYP b) {                          \
    union ctypes_complex_ ## UNION u;                                                 \
    union ctypes_complex_ ## UNION ua; union ctypes_complex_ ## UNION ub;             \
    ua.z = a; ub.z = b;                                                               \
    u.parts[0] = ua.parts[0] OP ub.parts[0]; u.parts[1] = ua.parts[1] OP ub.parts[1]; \
    return u.z; }
# define CCC_PRIM_MUL_OP(OPNAME, TYP, MSVCOP)                                          \
  static inline TYP ctypes_compat_ ## OPNAME(TYP a, TYP b) { return MSVCOP(a, b); }
# define CCC_PRIM_DIV_OP(OPNAME, TYP)                                                  \
  static inline TYP ctypes_compat_ ## OPNAME(TYP a, TYP b)                             \
  { caml_failwith("ctypes: OPNAME does not exist on current platform"); }
# define CCC_PRIM_INV_OP(OPNAME, TYP, CONSTRUCTOR, MSVCPOW)                            \
  static inline TYP ctypes_compat_ ## OPNAME(TYP a)                                    \
  { return MSVCPOW(a, CONSTRUCTOR(-1, 0)); }
#else
# define CCC_PRIM_COMPONENT_OP(OPNAME, TYP, UNION, OP)                                 \
  static inline TYP ctypes_compat_ ## OPNAME(TYP a, TYP b) { return a OP b; }
# define CCC_PRIM_MUL_OP(OPNAME, TYP, MSVCOP)                                          \
  static inline TYP ctypes_compat_ ## OPNAME(TYP a, TYP b) { return a * b; }
# define CCC_PRIM_DIV_OP(OPNAME, TYP)                                                  \
  static inline TYP ctypes_compat_ ## OPNAME(TYP a, TYP b) { return a / b; }
# define CCC_PRIM_INV_OP(OPNAME, TYP, CONSTRUCTOR, MSVCPOW)                                     \
  static inline TYP ctypes_compat_ ## OPNAME(TYP a) { return CONSTRUCTOR(1, 0) / a; }
#endif
CCC_PRIM_COMPONENT_OP(cadd, ctypes_complex_double, double_union, +)
CCC_PRIM_COMPONENT_OP(csub, ctypes_complex_double, double_union, -)
CCC_PRIM_MUL_OP(cmul, ctypes_complex_double, _Cmulcc)
CCC_PRIM_DIV_OP(cdiv, ctypes_complex_double)
CCC_PRIM_INV_OP(cinv, ctypes_complex_double, ctypes_compat_make_complex, cpow)
CCC_PRIM_COMPONENT_OP(caddf, ctypes_complex_float, float_union, +)
CCC_PRIM_COMPONENT_OP(csubf, ctypes_complex_float, float_union, -)
CCC_PRIM_MUL_OP(cmulf, ctypes_complex_float, _FCmulcc)
CCC_PRIM_DIV_OP(cdivf, ctypes_complex_float)
CCC_PRIM_INV_OP(cinvf, ctypes_complex_float, ctypes_compat_make_complexf, cpowf)
CCC_PRIM_COMPONENT_OP(caddl, ctypes_complex_long_double, long_double_union, +)
CCC_PRIM_COMPONENT_OP(csubl, ctypes_complex_long_double, long_double_union, -)
CCC_PRIM_MUL_OP(cmull, ctypes_complex_long_double, _LCmulcc)
CCC_PRIM_DIV_OP(cdivl, ctypes_complex_long_double)
CCC_PRIM_INV_OP(cinvl, ctypes_complex_long_double, ctypes_compat_make_complexl, cpowl)
#undef CCC_PRIM_OP

#endif /* CTYPES_COMPLEX_COMPATIBILITY_H */
