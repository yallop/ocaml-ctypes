/*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 */

#ifndef COMPLEX_STUBS_H
#define COMPLEX_STUBS_H

#include <complex.h>
#include <caml/mlvalues.h>

/* ctypes_copy_float_complex : float complex -> Complex.t */
value ctypes_copy_float_complex(float complex);

/* ctypes_copy_double_complex : double complex -> Complex.t */
value ctypes_copy_double_complex(double complex);

/* ctypes_float_complex_val : Complex.t -> float complex */
float complex ctypes_float_complex_val(value);

/* ctypes_double_complex_val : Complex.t -> double complex */
double complex ctypes_double_complex_val(value);

#endif /* COMPLEX_STUBS_H */
