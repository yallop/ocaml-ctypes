/*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 */

#ifndef CTYPES_UNSIGNED_STUBS_H
#define CTYPES_UNSIGNED_STUBS_H

#include <caml/mlvalues.h>

#include <stdint.h>

#define UINT_DECLS(BITS)                                                  \
  extern value ctypes_copy_uint ## BITS(uint ## BITS ## _t u);            \
  /* uintX_add : t -> t -> t */                                           \
  extern value ctypes_uint ## BITS ## _ ## add(value a, value b);         \
  /* uintX_sub : t -> t -> t */                                           \
  extern value ctypes_uint ## BITS ## _ ## sub(value a, value b);         \
  /* uintX_mul : t -> t -> t */                                           \
  extern value ctypes_uint ## BITS ## _ ## mul(value a, value b);         \
  /* uintX_div : t -> t -> t */                                           \
  extern value ctypes_uint ## BITS ## _ ## div(value a, value b);         \
  /* uintX_rem : t -> t -> t */                                           \
  extern value ctypes_uint ## BITS ## _ ## rem(value a, value b);         \
  /* uintX_logand : t -> t -> t */                                        \
  extern value ctypes_uint ## BITS ## _ ## logand(value a, value b);      \
  /* uintX_logor : t -> t -> t */                                         \
  extern value ctypes_uint ## BITS ## _ ## logor(value a, value b);       \
  /* uintX_logxor : t -> t -> t */                                        \
  extern value ctypes_uint ## BITS ## _ ## logxor(value a, value b);      \
  /* uintX_shift_left : t -> t -> t */                                    \
  extern value ctypes_uint ## BITS ## _ ## shift_left(value a, value b);  \
  /* uintX_shift_right : t -> t -> t */                                   \
  extern value ctypes_uint ## BITS ## _ ## shift_right(value a, value b); \
  /* of_int : int -> t */                                                 \
  extern value ctypes_uint ## BITS ## _of_int(value a);                   \
  /* to_int : t -> int */                                                 \
  extern value ctypes_uint ## BITS ## _to_int(value a);                   \
  /* of_string : string -> t */                                           \
  extern value ctypes_uint ## BITS ## _of_string(value a, value b);       \
  /* to_string : t -> string */                                           \
  extern value ctypes_uint ## BITS ## _to_string(value a);                \
  /* max : unit -> t */                                                   \
  extern value ctypes_uint ## BITS ## _max(value a);

UINT_DECLS(8)
UINT_DECLS(16)
UINT_DECLS(32)
UINT_DECLS(64)

/* X_size : unit -> int */
extern value ctypes_size_t_size (value _);
extern value ctypes_ushort_size (value _);
extern value ctypes_uint_size (value _);
extern value ctypes_ulong_size (value _);
extern value ctypes_ulonglong_size (value _);

#define Uint8_val(V) (*((uint8_t *) Data_custom_val(V)))
#define Uint16_val(V) (*((uint16_t *) Data_custom_val(V)))
#define Uint32_val(V) (*((uint32_t *) Data_custom_val(V)))
#define Uint64_val(V) (*((uint64_t *) Data_custom_val(V)))

#endif /* CTYPES_UNMSIGNED_STUBS_H */
