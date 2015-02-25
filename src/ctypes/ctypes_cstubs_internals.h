/*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 */

#ifndef CTYPES_CSTUBS_INTERNALS_H
#define CTYPES_CSTUBS_INTERNALS_H

/* Types and functions used by generated C code. */

#include "ctypes_primitives.h"
#include "ctypes_complex_stubs.h"
#include "ctypes_raw_pointer.h"
#include "ctypes_managed_buffer_stubs.h"
#include <caml/threads.h>
#define CTYPES_PTR_OF_OCAML_STRING(s) \
  (String_val(Field(s, 1)) + Int_val(Field(s, 0)))

#endif /* CTYPES_CSTUBS_INTERNALS_H */
