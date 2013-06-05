/*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 */

#ifndef RAW_POINTER_STUBS_H
#define RAW_POINTER_STUBS_H

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <stdint.h>

#if SIZEOF_PTR == 4
#define CTYPES_FROM_PTR(P) caml_copy_int32((intptr_t)P)
#define CTYPES_TO_PTR(I32) ((void *)Int32_val(I32))
#define CTYPES_PTR_PLUS(I32, I) caml_copy_int32(Int32_val(I32) + I)
#elif SIZEOF_PTR == 8
#define CTYPES_FROM_PTR(P) caml_copy_int64((intptr_t)P)
#define CTYPES_TO_PTR(I64) ((void *)Int64_val(I64))
#define CTYPES_PTR_PLUS(I64, I) caml_copy_int64(Int64_val(I64) + I)
#else
#error "No suitable type available to represent pointers."
#endif

#endif /* RAW_POINTER_STUBS_H */
