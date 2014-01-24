/*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 */

#ifndef MANAGED_BUFFER_STUBS_H
#define MANAGED_BUFFER_STUBS_H

#include <caml/mlvalues.h>

/* copy_bytes : void * -> size_t -> managed_buffer */
extern value ctypes_copy_bytes(void *, size_t);

/* allocate : int -> managed_buffer */
extern value ctypes_allocate(value size);

/* block_address : managed_buffer -> immediate_pointer */
extern value ctypes_block_address(value managed_buffer);

#endif /* MANAGED_BUFFER_STUBS_H */
