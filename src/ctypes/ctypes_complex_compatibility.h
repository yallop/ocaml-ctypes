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
  longdoublecomplex_t z;
  long double parts[2];
};

union ctypes_complex_double_union {
  doublecomplex_t z;
  double parts[2];
};

union ctypes_complex_float_union {
  floatcomplex_t z;
  float parts[2];
};

#if defined(__ANDROID__)
#define CTYPES_USE_STRUCT_BUILDER 1

#include <math.h>

static inline long double ctypes_compat_creall(longdoublecomplex_t z)
{ union ctypes_complex_long_double_union u; u.z = z; return u.parts[0]; }

static inline long double ctypes_compat_cimagl(longdoublecomplex_t z)
{ union ctypes_complex_long_double_union u; u.z = z; return u.parts[1]; }

static inline longdoublecomplex_t ctypes_compat_conjl(longdoublecomplex_t z)
{ union ctypes_complex_long_double_union u; u.z = z; u.parts[1] = -u.parts[1]; return u.z; }

static inline long double ctypes_compat_cargl(longdoublecomplex_t z)
{ return atan2(ctypes_compat_cimagl(z), ctypes_compat_creall(z)); }

static inline double ctypes_compat_creal(doublecomplex_t z)
{ union ctypes_complex_double_union u; u.z = z; return u.parts[0]; }

static inline double ctypes_compat_cimag(doublecomplex_t z)
{ union ctypes_complex_double_union u; u.z = z; return u.parts[1]; }

static inline doublecomplex_t ctypes_compat_conj(doublecomplex_t z)
{ union ctypes_complex_double_union u; u.z = z; u.parts[1] = -u.parts[1]; return u.z; }

static inline float ctypes_compat_crealf(floatcomplex_t z)
{ union ctypes_complex_float_union u; u.z = z; return u.parts[0]; }

static inline float ctypes_compat_cimagf(floatcomplex_t z)
{ union ctypes_complex_float_union u; u.z = z; return u.parts[1]; }

static inline floatcomplex_t ctypes_compat_conjf(floatcomplex_t z)
{ union ctypes_complex_float_union u; u.z = z; u.parts[1] = -u.parts[1]; return u.z; }

/* Android: As of API level 24, these functions do not exist. */

static inline longdoublecomplex_t ctypes_compat_csqrtl(longdoublecomplex_t z)
{ caml_failwith("ctypes: csqrtl does not exist on current platform"); }

static inline longdoublecomplex_t ctypes_compat_cexpl(longdoublecomplex_t z)
{ caml_failwith("ctypes: cexpl does not exist on current platform"); }

static inline longdoublecomplex_t ctypes_compat_clogl(longdoublecomplex_t z)
{ caml_failwith("ctypes: clogl does not exist on current platform"); }

static inline longdoublecomplex_t ctypes_compat_cpowl(longdoublecomplex_t x, longdoublecomplex_t z)
{ caml_failwith("ctypes: cpowl does not exist on current platform"); }


#else

#include <complex.h>

static inline long double ctypes_compat_creall(longdoublecomplex_t z)
{ return creall(z); }
static inline long double ctypes_compat_cimagl(longdoublecomplex_t z)
{ return cimagl(z); }
static inline longdoublecomplex_t ctypes_compat_conjl(longdoublecomplex_t z)
{ return conjl(z); }

#if defined(__FreeBSD__)
static inline longdoublecomplex_t ctypes_compat_cexpl(longdoublecomplex_t z)
{ caml_failwith("ctypes: cexpl does not exist on current platform"); }
#else
static inline longdoublecomplex_t ctypes_compat_cexpl(longdoublecomplex_t z)
{ return cexpl(z); }
#endif
static inline longdoublecomplex_t ctypes_compat_clogl(longdoublecomplex_t z)
{ return clogl(z); }
static inline longdoublecomplex_t ctypes_compat_cpowl(longdoublecomplex_t x, longdoublecomplex_t z)
{ return cpowl(x, z); }

static inline longdoublecomplex_t ctypes_compat_csqrtl(longdoublecomplex_t z)
{ return csqrtl(z); }
static inline long double ctypes_compat_cargl(longdoublecomplex_t z)
{ return cargl(z); }

static inline double ctypes_compat_creal(doublecomplex_t z)
{ return creal(z); }
static inline double ctypes_compat_cimag(doublecomplex_t z)
{ return cimag(z); }
static inline doublecomplex_t ctypes_compat_conj(doublecomplex_t z)
{ return conj(z); }

static inline float ctypes_compat_crealf(floatcomplex_t z)
{ return crealf(z); }
static inline float ctypes_compat_cimagf(floatcomplex_t z)
{ return cimagf(z); }
static inline floatcomplex_t ctypes_compat_conjf(floatcomplex_t z)
{ return conjf(z); }

#if !defined(CMPLXF) || !defined(CMPLX) || !defined(CMPLXL)
#define CTYPES_USE_STRUCT_BUILDER 1
#else
static inline doublecomplex_t ctypes_compat_make_complex(double re, double im)
{ return (CMPLX(re,im)); }
static inline longdoublecomplex_t ctypes_compat_make_complexl(long double re, long double im)
{ return (CMPLXL(re,im)); }
static inline floatcomplex_t ctypes_compat_make_complexf(float re, float im)
{ return (CMPLXF(re,im)); }
#endif

#endif

#ifdef CTYPES_USE_STRUCT_BUILDER
static inline doublecomplex_t ctypes_compat_make_complex(double re, double im)
{ union ctypes_complex_double_union u; u.parts[0] = re; u.parts[1] = im; return u.z; }
static inline floatcomplex_t ctypes_compat_make_complexf(float re, float im)
{ union ctypes_complex_float_union u; u.parts[0] = re; u.parts[1] = im; return u.z; }
static inline longdoublecomplex_t ctypes_compat_make_complexl(long double re, long double im)
{ union ctypes_complex_long_double_union u; u.parts[0] = re; u.parts[1] = im; return u.z; }
#undef CTYPES_USE_STRUCT_BUILDER
#endif

// Primitive arithmetic
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
#else
# define CCC_PRIM_COMPONENT_OP(OPNAME, TYP, UNION, OP)                                 \
  static inline TYP ctypes_compat_ ## OPNAME(TYP a, TYP b) { return a OP b; }
# define CCC_PRIM_MUL_OP(OPNAME, TYP, MSVCOP)                                          \
  static inline TYP ctypes_compat_ ## OPNAME(TYP a, TYP b) { return a * b; }
# define CCC_PRIM_DIV_OP(OPNAME, TYP)                                                  \
  static inline TYP ctypes_compat_ ## OPNAME(TYP a, TYP b) { return a / b; }
#endif
CCC_PRIM_COMPONENT_OP(cadd, doublecomplex_t, double_union, +)
CCC_PRIM_COMPONENT_OP(csub, doublecomplex_t, double_union, -)
CCC_PRIM_MUL_OP(cmul, doublecomplex_t, _Cmulcc)
CCC_PRIM_DIV_OP(cdiv, doublecomplex_t)
CCC_PRIM_COMPONENT_OP(caddf, floatcomplex_t, float_union, +)
CCC_PRIM_COMPONENT_OP(csubf, floatcomplex_t, float_union, -)
CCC_PRIM_MUL_OP(cmulf, floatcomplex_t, _FCmulcc)
CCC_PRIM_DIV_OP(cdivf, floatcomplex_t)
CCC_PRIM_COMPONENT_OP(caddl, longdoublecomplex_t, long_double_union, +)
CCC_PRIM_COMPONENT_OP(csubl, longdoublecomplex_t, long_double_union, -)
CCC_PRIM_MUL_OP(cmull, longdoublecomplex_t, _LCmulcc)
CCC_PRIM_DIV_OP(cdivl, longdoublecomplex_t)
#undef CCC_PRIM_OP

#endif /* CTYPES_COMPLEX_COMPATIBILITY_H */
