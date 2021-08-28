/*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 */

#ifndef CTYPES_COMPLEX_STUBS_H
#define CTYPES_COMPLEX_STUBS_H

#include "ctypes_complex_types.h"
#include <caml/mlvalues.h>

/* ctypes_copy_float_complex : float _Complex -> Complex.t */
value ctypes_copy_float_complex(floatcomplex_t);

/* ctypes_copy_double_complex : double _Complex -> Complex.t */
value ctypes_copy_double_complex(doublecomplex_t);

/* ctypes_float_complex_val : Complex.t -> float _Complex */
floatcomplex_t ctypes_float_complex_val(value);

/* ctypes_double_complex_val : Complex.t -> double _Complex */
doublecomplex_t ctypes_double_complex_val(value);

#endif /* CTYPES_COMPLEX_STUBS_H */
