/*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 */
#define _XOPEN_SOURCE 500
#include <caml/mlvalues.h>

#include <assert.h>

#include <sys/types.h>
#include <unistd.h>
#include <signal.h>
#if (!defined _WIN32 || defined __CYGWIN__) && !defined MINIOS
#include <pthread.h>
#endif
#include <time.h>

#include <stdint.h>

enum arithmetic {
  Int8,
  Int16,
  Int32,
  Int64,
  Uint8,
  Uint16,
  Uint32,
  Uint64,
  Float,
  Double,
};

#define FLOATING_FLAG_BIT 15
#define UNSIGNED_FLAG_BIT 14
#define FLOATING ((size_t)1u << FLOATING_FLAG_BIT)
#define UNSIGNED ((size_t)1u << UNSIGNED_FLAG_BIT)
#define CHECK_FLOATING(TYPENAME) \
  ((unsigned)(((TYPENAME) 0.5) != 0) << FLOATING_FLAG_BIT)
#define CHECK_UNSIGNED(TYPENAME) \
  ((unsigned)(((TYPENAME) -1) > 0) << UNSIGNED_FLAG_BIT)
#define CLASSIFY(TYPENAME) (CHECK_FLOATING(TYPENAME) | CHECK_UNSIGNED(TYPENAME))
#define ARITHMETIC_TYPEINFO(TYPENAME) (CLASSIFY(TYPENAME) | sizeof(TYPENAME))

static enum arithmetic _underlying_type(size_t typeinfo)
{
  switch (typeinfo)
  {
  case FLOATING | sizeof(float):    return Float;
  case FLOATING | sizeof(double):   return Double;
  case UNSIGNED | sizeof(uint8_t):  return Uint8;
  case UNSIGNED | sizeof(uint16_t): return Uint16;
  case UNSIGNED | sizeof(uint32_t): return Uint32;
  case UNSIGNED | sizeof(uint64_t): return Uint64;
  case            sizeof(int8_t):   return Int8;
  case            sizeof(int16_t):  return Int16;
  case            sizeof(int32_t):  return Int32;
  case            sizeof(int64_t):  return Int64;
  default: assert(0);
  }
}

#define EXPOSE_TYPEINFO_COMMON(TYPENAME,STYPENAME)           \
  value ctypes_typeof_ ## TYPENAME(value unit)               \
  {                                                          \
    size_t typeinfo = ARITHMETIC_TYPEINFO(STYPENAME);        \
    enum arithmetic underlying = _underlying_type(typeinfo); \
    return Val_int(underlying);                              \
  }

#define EXPOSE_ALIGNMENT_COMMON(TYPENAME,STYPENAME)          \
  value ctypes_alignmentof_ ## TYPENAME(value unit)          \
  {                                                          \
    struct s { char c; STYPENAME t; };                       \
    return Val_int(offsetof(struct s, t));                   \
  }

#define EXPOSE_TYPESIZE_COMMON(TYPENAME,STYPENAME)           \
  value ctypes_sizeof_ ## TYPENAME(value unit)               \
  {                                                          \
    return Val_int(sizeof(STYPENAME));                       \
  }

#if !defined _WIN32 || defined __CYGWIN__
  #define UNDERSCORE(X) X
#else
  #define UNDERSCORE(X) _## X
#endif

#define EXPOSE_TYPEINFO(X) EXPOSE_TYPEINFO_COMMON(X, X)
#define EXPOSE_TYPEINFO_S(X) EXPOSE_TYPEINFO_COMMON(X, UNDERSCORE(X))
#define EXPOSE_TYPESIZE(X) EXPOSE_TYPESIZE_COMMON(X, X)
#define EXPOSE_TYPESIZE_S(X) EXPOSE_TYPESIZE_COMMON(X, UNDERSCORE(X))
#define EXPOSE_ALIGNMENT(X) EXPOSE_ALIGNMENT_COMMON(X, X)
#define EXPOSE_ALIGNMENT_S(X) EXPOSE_ALIGNMENT_COMMON(X, UNDERSCORE(X))

EXPOSE_TYPEINFO(clock_t)
EXPOSE_TYPEINFO_S(dev_t)
EXPOSE_TYPEINFO_S(ino_t)
EXPOSE_TYPEINFO_S(mode_t)
EXPOSE_TYPEINFO_S(off_t)
EXPOSE_TYPEINFO_S(pid_t)
EXPOSE_TYPEINFO(ssize_t)
EXPOSE_TYPEINFO(time_t)
EXPOSE_TYPEINFO(useconds_t)
#if !defined _WIN32 || defined __CYGWIN__
  EXPOSE_TYPEINFO(nlink_t)
#else
  /* the mingw port of fts uses an int for nlink_t */
  EXPOSE_TYPEINFO_COMMON(nlink_t, int)
#endif


EXPOSE_TYPESIZE_S(sigset_t)
EXPOSE_ALIGNMENT_S(sigset_t)
