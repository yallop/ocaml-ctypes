#include <caml/mlvalues.h>
#include <caml/custom.h>
#include <caml/alloc.h>
#include <caml/intext.h>
#include <caml/fail.h>

#include <inttypes.h>
#include <stdint.h>
#include <limits.h>
#include <stdio.h>

#define Uint_custom_val(TYPE, V) (*((TYPE *) Data_custom_val(V)))
#define TYPE(SIZE) uint ## SIZE ## _t
#define BYTES(SIZE) (SIZE / CHAR_BIT)
#define BUF_SIZE(TYPE) ((sizeof(TYPE) * CHAR_BIT + 2) / 3 + 1)

#define UINT_PRIMOP(NAME, SIZE, OP)                                     \
  /* OP : t -> t -> t */                                                   \
  value ctypes_uint ## SIZE ## _ ## NAME(value a, value b)                 \
  {                                                                        \
    return caml_copy_uint ## SIZE(Uint_custom_val(uint ## SIZE ## _t, a)   \
                               OP Uint_custom_val(uint ## SIZE ## _t, b)); \
  }

#define UINT_DEFS(BITS, BYTES)                                               \
  static int uint ## BITS ## _cmp(value v1, value v2)                        \
  {                                                                          \
    TYPE(BITS) u1 = Uint_custom_val(TYPE(BITS), v1);                         \
    TYPE(BITS) u2 = Uint_custom_val(TYPE(BITS), v2);                         \
    return (u1 > u2) - (u1 < u2);                                            \
  }                                                                          \
                                                                             \
  static long uint ## BITS ## _hash(value v)                                 \
  {                                                                          \
    return Uint_custom_val(TYPE(BITS), v);                                   \
  }                                                                          \
                                                                             \
  static void uint ## BITS ## _serialize(value v,                            \
                                         unsigned long *wsize_32,            \
                                         unsigned long *wsize_64)            \
  {                                                                          \
    caml_serialize_int_ ## BYTES(Uint_custom_val(TYPE(BITS), v));            \
    *wsize_32 = *wsize_64 = BYTES;                                           \
  }                                                                          \
                                                                             \
  static unsigned long uint ## BITS ## _deserialize(void *dst)               \
  {                                                                          \
    *(TYPE(BITS) *)dst = caml_deserialize_uint_ ## BYTES();                  \
    return BYTES;                                                            \
  }                                                                          \
                                                                             \
  static struct custom_operations caml_uint ## BITS ## _ops = {              \
    "ctypes:uint" #BITS,                                                     \
    custom_finalize_default,                                                 \
    uint ## BITS ## _cmp,                                                    \
    uint ## BITS ## _hash,                                                   \
    uint ## BITS ## _serialize,                                              \
    uint ## BITS ## _deserialize,                                            \
    custom_compare_ext_default                                               \
  };                                                                         \
                                                                             \
  value caml_copy_uint ## BITS(TYPE(BITS) u)                                 \
  {                                                                          \
    value res = caml_alloc_custom(&caml_uint ## BITS ## _ops, 4, 0, 1);      \
    Uint_custom_val(TYPE(BITS), res) = u;                                    \
    return res;                                                              \
  }                                                                          \
  UINT_PRIMOP(add, BITS,  +)                                                 \
  UINT_PRIMOP(sub, BITS,  -)                                                 \
  UINT_PRIMOP(mul, BITS,  *)                                                 \
  UINT_PRIMOP(div, BITS,  /)                                                 \
  UINT_PRIMOP(rem, BITS,  %)                                                 \
  UINT_PRIMOP(logand, BITS,  &)                                              \
  UINT_PRIMOP(logor, BITS,  |)                                               \
  UINT_PRIMOP(logxor, BITS,  ^)                                              \
  UINT_PRIMOP(shift_left, BITS,  <<)                                         \
  UINT_PRIMOP(shift_right, BITS,  >>)                                        \
                                                                             \
  /* of_int : int -> t */                                                    \
  value ctypes_uint ## BITS ## _of_int(value a)                              \
  {                                                                          \
    return caml_copy_uint ## BITS (Int_val(a));                              \
  }                                                                          \
                                                                             \
  /* to_int : t -> int */                                                    \
  value ctypes_uint ## BITS ## _to_int(value a)                              \
  {                                                                          \
    return Val_int(Uint_custom_val(TYPE(BITS), a));                          \
  }                                                                          \
                                                                             \
  /* of_string : string -> t */                                              \
  value ctypes_uint ## BITS ## _of_string(value a, value b)                  \
  {                                                                          \
    TYPE(BITS) u;                                                            \
    if (sscanf(String_val(a), "%" SCNu ## BITS , &u) != 1)                   \
      caml_failwith("int_of_string");                                        \
    else                                                                     \
      return caml_copy_uint ## BITS (u);                                     \
  }                                                                          \
                                                                             \
  /* to_string : t -> string */                                              \
  value ctypes_uint ## BITS ## _to_string(value a)                           \
  {                                                                          \
    char buf[BUF_SIZE(TYPE(BITS))];                                          \
    if (sprintf(buf, "%" PRIu ## BITS , Uint_custom_val(TYPE(BITS), a)) < 0) \
      caml_failwith("string_of_int");                                        \
    else                                                                     \
      return caml_copy_string(buf);                                          \
  }                                                                          \
                                                                             \
  /* max : unit -> t */                                                      \
  value ctypes_uint ## BITS ## _max(value a)                                 \
  {                                                                          \
    return caml_copy_uint ## BITS ((TYPE(BITS))(-1));                        \
  }                                                                          \


UINT_DEFS(8, 1)
UINT_DEFS(16, 2)
UINT_DEFS(32, 4)
UINT_DEFS(64, 8)

value ctypes_size_t_size (value _) { return Val_int(sizeof (size_t)); }
value ctypes_ushort_size (value _) { return Val_int(sizeof (unsigned short)); }
value ctypes_uint_size (value _) { return Val_int(sizeof (unsigned int)); }
value ctypes_ulong_size (value _) { return Val_int(sizeof (unsigned long)); }
value ctypes_ulonglong_size (value _) { return Val_int(sizeof (unsigned long long)); }
