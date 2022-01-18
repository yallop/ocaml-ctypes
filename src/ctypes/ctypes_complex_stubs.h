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
value ctypes_copy_float_complex(ctypes_complex_float);

/* ctypes_copy_double_complex : double _Complex -> Complex.t */
value ctypes_copy_double_complex(ctypes_complex_double);

/* ctypes_float_complex_val : Complex.t -> float _Complex */
ctypes_complex_float ctypes_float_complex_val(value);

/* ctypes_double_complex_val : Complex.t -> double _Complex */
ctypes_complex_double ctypes_double_complex_val(value);

#endif /* CTYPES_COMPLEX_STUBS_H */
