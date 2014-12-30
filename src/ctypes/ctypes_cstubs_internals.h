/*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 */

#ifndef CSTUBS_INTERNALS_H
#define CSTUBS_INTERNALS_H

/* Types and functions used by generated C code. */

#include "ctypes/ctypes_primitives.h"
#include "ctypes/ctypes_complex_stubs.h"
#include "ctypes/ctypes_raw_pointer.h"
#include "ctypes/ctypes_managed_buffer_stubs.h"
#define CTYPES_PTR_OF_OCAML_STRING(s) \
  (String_val(Field(s, 1)) + Int_val(Field(s, 0)))

#endif /* CSTUBS_INTERNALS_H */
