/*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 */

#ifndef CTYPES_PRIMITIVES_H
#define CTYPES_PRIMITIVES_H

#include <limits.h>
#include <stdint.h>

#include "unsigned_stubs.h"

/* The order here must correspond to the constructor order in primitives.ml */
enum ctypes_primitive {
  Char,
  Schar,
  Uchar,
  Short,
  Int,
  Long,
  Llong,
  Ushort,
  Uint,
  Ulong,
  Ullong,
  Size_t,
  Int8_t,
  Int16_t,
  Int32_t,
  Int64_t,
  Uint8_t,
  Uint16_t,
  Uint32_t,
  Uint64_t,
  Camlint,
  Nativeint,
  Float,
  Double,
  Complex32,
  Complex64,
};

/* short is at least 16 bits. */
#if USHRT_MAX == UINT16_MAX
#define ctypes_ushort_val Uint16_val
#define ctypes_copy_ushort ctypes_copy_uint16
#elif USHRT_MAX == UINT32_MAX
#define ctypes_ushort_val Uint32_val
#define ctypes_copy_ushort ctypes_copy_uint32
#elif USHRT_MAX == UINT64_MAX
#define ctypes_ushort_val Uint64_val
#define ctypes_copy_ushort ctypes_copy_uint64
#else
# error "No suitable OCaml type available for representing unsigned short values"
#endif

/* int is at least 16 bits. */
#if UINT_MAX == UINT16_MAX
#define ctypes_uint_val Uint16_val
#define ctypes_copy_uint ctypes_copy_uint16
#elif UINT_MAX == UINT32_MAX
#define ctypes_uint_val Uint32_val
#define ctypes_copy_uint ctypes_copy_uint32
#elif UINT_MAX == UINT64_MAX
#define ctypes_uint_val Uint64_val
#define ctypes_copy_uint ctypes_copy_uint64
#else
# error "No suitable OCaml type available for representing unsigned int values"
#endif

/* long is at least 16 bits. */
#if ULONG_MAX == UINT32_MAX
#define ctypes_long_val Int32_val
#define ctypes_ulong_val Uint32_val
#define ctypes_copy_long caml_copy_int32
#define ctypes_copy_ulong ctypes_copy_uint32
#elif ULONG_MAX == UINT64_MAX
#define ctypes_long_val Int64_val
#define ctypes_ulong_val Uint64_val
#define ctypes_copy_long caml_copy_int64
#define ctypes_copy_ulong ctypes_copy_uint64
#else
# error "No suitable OCaml type available for representing longs"
#endif

/* long long is at least 16 bits. */
#if ULLONG_MAX == UINT64_MAX
#define ctypes_llong_val Int64_val
#define ctypes_ullong_val Uint64_val
#define ctypes_copy_llong caml_copy_int64
#define ctypes_copy_ullong ctypes_copy_uint64
#else
# error "No suitable OCaml type available for representing long longs"
#endif

#if SIZE_MAX == UINT16_MAX
#define ctypes_size_t_val Uint16_val
#define ctypes_copy_size_t ctypes_copy_uint16
#elif SIZE_MAX == UINT32_MAX
#define ctypes_size_t_val Uint32_val
#define ctypes_copy_size_t ctypes_copy_uint32
#elif SIZE_MAX == UINT64_MAX
#define ctypes_size_t_val Uint64_val
#define ctypes_copy_size_t ctypes_copy_uint64
#else
# error "No suitable OCaml type available for representing size_t values"
#endif

#endif /* CTYPES_PRIMITIVES_H */
