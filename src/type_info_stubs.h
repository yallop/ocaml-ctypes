/*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 */

#ifndef TYPE_INFO_STUBS_H
#define TYPE_INFO_STUBS_H

#include <caml/mlvalues.h>
#include <caml/fail.h>
#include <caml/callback.h>

#include <ffi.h>

struct type_info {
  const char *name;
  enum { PASSABLE, UNPASSABLE, STRUCT } passable;
  ffi_type   *ffitype;
  value     (*raw_read)(struct type_info *, void *);
  value     (*raw_write)(struct type_info *, void *, value);
};

/* allocate_struct_type_info : ffitype*** -> _ ctype */
value ctypes_allocate_struct_type_info(ffi_type ***args);

/* X_type_info : unit -> _ ctype */
extern value ctypes_int8_t_type_info(value unit);
extern value ctypes_int16_t_type_info(value unit);
extern value ctypes_int32_t_type_info(value unit);
extern value ctypes_int64_t_type_info(value unit);
extern value ctypes_double_type_info(value unit);
extern value ctypes_float_type_info(value unit);
extern value ctypes_nativeint_type_info(value unit);
extern value ctypes_voidp_type_info(value unit);
extern value ctypes_int_type_info(value unit);
extern value ctypes_short_type_info(value unit);
extern value ctypes_schar_type_info(value unit);
extern value ctypes_uchar_type_info(value unit);
extern value ctypes_char_type_info(value unit);
extern value ctypes_string_type_info(value unit);
extern value ctypes_uint8_t_type_info(value unit);
extern value ctypes_uint16_t_type_info(value unit);
extern value ctypes_uint32_t_type_info(value unit);
extern value ctypes_uint64_t_type_info(value unit);
extern value ctypes_ushort_type_info(value unit);
extern value ctypes_uint_type_info(value unit);
extern value ctypes_ulong_type_info(value unit);
extern value ctypes_ullong_type_info(value unit);
extern value ctypes_long_type_info(value unit);
extern value ctypes_llong_type_info(value unit);
extern value ctypes_size_t_type_info(value unit);
extern value ctypes_void_type_info(value unit);

/* sizeof : _ctype -> int */
extern value ctypes_sizeof(value ctype);

/* alignment : _ ctype -> int */
extern value ctypes_alignment(value ctype);

/* Read a C value from a block of memory */
/* read : 'a ctype -> offset:int -> immediate_pointer -> 'a */
extern value ctypes_read(value ctype, value offset, value buffer);

/* Write a C value to a block of memory */
/* write : 'a ctype -> offset:int -> 'a -> immediate_pointer -> unit */
extern value ctypes_write(value ctype, value offset, value v, value buffer);

#endif /* TYPE_INFO_STUBS_H */
