/*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 */

#include <complex.h>

#include <caml/memory.h>
#include <caml/alloc.h>

static value allocate_complex_value(double r, double i)
{
  value v = caml_alloc(2 * sizeof(double), Double_array_tag);
  Store_double_field(v, 0, r);
  Store_double_field(v, 1, i);
  return v;
}

/* ctypes_copy_float_complex : float complex -> Complex.t */
value ctypes_copy_float_complex(float complex c)
{
  return allocate_complex_value(crealf(c), cimagf(c));
}

/* ctypes_copy_double_complex : double complex -> Complex.t */
value ctypes_copy_double_complex(double complex c)
{
  return allocate_complex_value(creal(c), cimag(c));
}

/* ctypes_float_complex_val : Complex.t -> float complex */
float complex ctypes_float_complex_val(value v)
{
  return Double_field(v, 0) + Double_field(v, 1) * I;
}

/* ctypes_double_complex_val : Complex.t -> double complex */
double complex ctypes_double_complex_val(value v)
{
  return Double_field(v, 0) + Double_field(v, 1) * I;
}
