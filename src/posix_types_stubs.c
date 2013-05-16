#include <caml/mlvalues.h>

#include <assert.h>

#include <sys/types.h>
#include <unistd.h>

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

#define FLOATING_FLAG_BIT 16
#define SIGNED_FLAG_BIT   15
#define FLOATING ((size_t)1u << FLOATING_FLAG_BIT)
#define SIGNED   ((size_t)1u << SIGNED_FLAG_BIT)
#define IS_FLOATING(TYPENAME) \
  ((unsigned)(((TYPENAME) 0.5) != 0) << FLOATING_FLAG_BIT)
#define IS_SIGNED(TYPENAME) \
  ((unsigned)(((TYPENAME) -1) == -1) << SIGNED_FLAG_BIT)
#define CLASSIFY(TYPENAME) (IS_FLOATING(TYPENAME) | IS_SIGNED(TYPENAME))
#define ARITHMETIC_TYPEINFO(TYPENAME) (CLASSIFY(TYPENAME) | sizeof(TYPENAME))

static enum arithmetic _underlying_type(size_t typeinfo)
{
  switch (typeinfo)
  {
  case FLOATING | sizeof(float):   return Float;
  case FLOATING | sizeof(double):  return Double;
  case SIGNED   | sizeof(int8_t):  return Int8;
  case SIGNED   | sizeof(int16_t): return Int16;
  case SIGNED   | sizeof(int32_t): return Int32;
  case SIGNED   | sizeof(int64_t): return Int64;
  case            sizeof(int8_t):  return Uint8;
  case            sizeof(int16_t): return Uint16;
  case            sizeof(int32_t): return Uint32;
  case            sizeof(int64_t): return Uint64;
  default: assert(0);
  }
}

#define EXPOSE_TYPEINFO(TYPENAME)                            \
  value ctypes_typeof_ ## TYPENAME(value unit)               \
  {                                                          \
    size_t typeinfo = ARITHMETIC_TYPEINFO(TYPENAME);         \
    enum arithmetic underlying = _underlying_type(typeinfo); \
    return Val_int(underlying);                              \
  }

#define EXPOSE_TYPESIZE(TYPENAME)              \
  value ctypes_sizeof_ ## TYPENAME(value unit) \
  {                                            \
    return Val_int(sizeof(TYPENAME));          \
  }

EXPOSE_TYPEINFO(blkcnt_t)
EXPOSE_TYPEINFO(blksize_t)
EXPOSE_TYPEINFO(clock_t)
EXPOSE_TYPEINFO(clockid_t)
EXPOSE_TYPEINFO(dev_t)
EXPOSE_TYPEINFO(fsblkcnt_t)
EXPOSE_TYPEINFO(fsfilcnt_t)
EXPOSE_TYPEINFO(gid_t)
EXPOSE_TYPEINFO(id_t)
EXPOSE_TYPEINFO(ino_t)
EXPOSE_TYPEINFO(mode_t)
EXPOSE_TYPEINFO(nlink_t)
EXPOSE_TYPEINFO(off_t)
EXPOSE_TYPEINFO(pid_t)
EXPOSE_TYPEINFO(pthread_t)
EXPOSE_TYPEINFO(ssize_t)
EXPOSE_TYPEINFO(suseconds_t)
EXPOSE_TYPEINFO(time_t)
/* The Open Group says timer_t should be an arithmetic type, but it's
   a pointer on Linux. */
/* EXPOSE_TYPEINFO(timer_t) */
EXPOSE_TYPEINFO(uid_t)
EXPOSE_TYPEINFO(useconds_t)

EXPOSE_TYPESIZE(key_t)
EXPOSE_TYPESIZE(pthread_attr_t)
EXPOSE_TYPESIZE(pthread_cond_t)
EXPOSE_TYPESIZE(pthread_condattr_t)
EXPOSE_TYPESIZE(pthread_key_t)
EXPOSE_TYPESIZE(pthread_mutex_t)
EXPOSE_TYPESIZE(pthread_mutexattr_t)
EXPOSE_TYPESIZE(pthread_once_t)
EXPOSE_TYPESIZE(pthread_rwlock_t)
EXPOSE_TYPESIZE(pthread_rwlockattr_t)
EXPOSE_TYPESIZE(timer_t)
