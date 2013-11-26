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

/* allocate_struct_type_info : ffitype*** -> _ ctype */
value ctypes_allocate_struct_type_info(ffi_type ***args);

/* allocate_unpassable_struct_type_info : (size, alignment) -> _ ctype */
value ctypes_allocate_unpassable_struct_type_info(int size, int alignment);

/* Read a C value from a block of memory */
/* read : 'a prim -> offset:int -> raw_pointer -> 'a */
extern value ctypes_read(value ctype, value offset, value buffer);

/* Write a C value to a block of memory */
/* write : 'a prim -> offset:int -> 'a -> raw_pointer -> unit */
extern value ctypes_write(value ctype, value offset, value v, value buffer);

#endif /* TYPE_INFO_STUBS_H */
