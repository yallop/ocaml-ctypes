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
#if !defined _WIN32 || defined __CYGWIN__
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

#define EXPOSE_TYPEINFO(TYPENAME) \
    EXPOSE_TYPEINFO_COMMON(TYPENAME,TYPENAME)

#if !defined _WIN32 || defined __CYGWIN__

#define EXPOSE_TYPEINFO_NO_WIN EXPOSE_TYPEINFO
#define EXPOSE_TYPEINFO_S EXPOSE_TYPEINFO

#else

#define EXPOSE_TYPEINFO_NO_WIN(TYPENAME)                     \
  value ctypes_typeof_ ## TYPENAME(value unit)               \
  {                                                          \
    return Val_int(sizeof(int64_t));                         \
  }
#define EXPOSE_TYPEINFO_S(X)                    \
    EXPOSE_TYPEINFO_COMMON(X,_## X )

#endif


EXPOSE_TYPEINFO_NO_WIN(blkcnt_t)
EXPOSE_TYPEINFO_NO_WIN(blksize_t)
EXPOSE_TYPEINFO(clock_t)
EXPOSE_TYPEINFO_S(dev_t)
EXPOSE_TYPEINFO_NO_WIN(fsblkcnt_t)
EXPOSE_TYPEINFO_NO_WIN(fsfilcnt_t)
EXPOSE_TYPEINFO_NO_WIN(gid_t)
EXPOSE_TYPEINFO_NO_WIN(id_t)
EXPOSE_TYPEINFO_S(ino_t)
EXPOSE_TYPEINFO_S(mode_t)
EXPOSE_TYPEINFO_NO_WIN(nlink_t)
EXPOSE_TYPEINFO_S(off_t)
EXPOSE_TYPEINFO_S(pid_t)
EXPOSE_TYPEINFO(ssize_t)
EXPOSE_TYPEINFO_NO_WIN(suseconds_t)
EXPOSE_TYPEINFO(time_t)
EXPOSE_TYPEINFO_NO_WIN(uid_t)
EXPOSE_TYPEINFO(useconds_t)

#if !defined _WIN32 || defined __CYGWIN__
#define EXPOSE_TYPESIZE(TYPENAME)              \
  value ctypes_sizeof_ ## TYPENAME(value unit) \
  {                                            \
    return Val_int(sizeof(TYPENAME));          \
  }
#else
#define EXPOSE_TYPESIZE(TYPENAME)              \
  value ctypes_sizeof_ ## TYPENAME(value unit) \
  {                                            \
    return Val_int(sizeof(int64_t));           \
  }
#endif

EXPOSE_TYPESIZE(key_t)
EXPOSE_TYPESIZE(pthread_t)
EXPOSE_TYPESIZE(pthread_attr_t)
EXPOSE_TYPESIZE(pthread_cond_t)
EXPOSE_TYPESIZE(pthread_condattr_t)
EXPOSE_TYPESIZE(pthread_key_t)
EXPOSE_TYPESIZE(pthread_mutex_t)
EXPOSE_TYPESIZE(pthread_mutexattr_t)
EXPOSE_TYPESIZE(pthread_once_t)
EXPOSE_TYPESIZE(pthread_rwlock_t)
EXPOSE_TYPESIZE(pthread_rwlockattr_t)

#if !defined _WIN32 || defined __CYGWIN__
EXPOSE_TYPESIZE(sigset_t)
#else
value ctypes_sizeof_sigset_t(value unit)
{
  return Val_int(sizeof(_sigset_t));
}
#endif

#if !defined _WIN32 || defined __CYGWIN__
#define EXPOSE_ALIGNMENT(TYPENAME)                  \
  value ctypes_alignmentof_ ## TYPENAME(value unit) \
  {                                                 \
    struct s { char c; TYPENAME t; };               \
    return Val_int(offsetof(struct s, t));          \
  }

#else
#define EXPOSE_ALIGNMENT(TYPENAME)                  \
  value ctypes_alignmentof_ ## TYPENAME(value unit) \
  {                                                 \
    struct s { char c; int64_t t; };                \
    return Val_int(offsetof(struct s, t));          \
  }
#endif

EXPOSE_ALIGNMENT(key_t)
EXPOSE_ALIGNMENT(pthread_t)
EXPOSE_ALIGNMENT(pthread_attr_t)
EXPOSE_ALIGNMENT(pthread_cond_t)
EXPOSE_ALIGNMENT(pthread_condattr_t)
EXPOSE_ALIGNMENT(pthread_key_t)
EXPOSE_ALIGNMENT(pthread_mutex_t)
EXPOSE_ALIGNMENT(pthread_mutexattr_t)
EXPOSE_ALIGNMENT(pthread_once_t)
EXPOSE_ALIGNMENT(pthread_rwlock_t)
EXPOSE_ALIGNMENT(pthread_rwlockattr_t)
#if !defined _WIN32 || defined __CYGWIN__
EXPOSE_ALIGNMENT(sigset_t)
#else
value ctypes_alignmentof_sigset_t(value unit)
{
  struct s { char c; _sigset_t t; };
  return Val_int(offsetof(struct s, t));
}
#endif
