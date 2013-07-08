/*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 */

#include <complex.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>

#include "type_info_stubs.h"

static value allocate_complex_value(double r, double i)
{
  value v = caml_alloc(2 * sizeof(double), Double_array_tag);
  Store_double_field(v, 0, r);
  Store_double_field(v, 1, i);
  return v;
}

/* float complex */
static value raw_read_float_complex(struct type_info * _, void *p)
{
  float complex c = *(float complex *)p;
  return allocate_complex_value(crealf(c), cimagf(c));
}

static value raw_write_float_complex(struct type_info * _, void *p, value v)
{
  CAMLparam1(v);
  double real = Double_field(v, 0);
  double imag = Double_field(v, 1);
  *(float complex *)p = real + imag * I;
  CAMLreturn(Val_unit);
}

struct float_complex_alignment {
  char c;
  float complex f;
};

/* initialized in ctypes_float_complex_type_info */
static ffi_type float_complex_ffi_type;

static struct type_info _float_complex_type_info = {
  "float complex",
  UNPASSABLE,
  &float_complex_ffi_type,
  raw_read_float_complex,
  raw_write_float_complex,
};

value ctypes_float_complex_type_info(value unit)
{
  /* It's not clear that the fields we're using here are part of the
     libffi public interface. */
  float_complex_ffi_type.size = sizeof (float complex);
  float_complex_ffi_type.alignment = offsetof
    (struct float_complex_alignment, f);
  float_complex_ffi_type.type = -1;
  float_complex_ffi_type.elements = NULL;

  return ctypes_allocate_type_info(&_float_complex_type_info);
}

/* double complex */
static value raw_read_double_complex(struct type_info * _, void *p)
{
  double complex c = *(double complex *)p;
  return allocate_complex_value(creal(c), cimag(c));
}

static value raw_write_double_complex(struct type_info * _, void *p, value v)
{
  CAMLparam1(v);
  double real = Double_field(v, 0);
  double imag = Double_field(v, 1);
  *(double complex *)p = real + imag * I;
  CAMLreturn(Val_unit);
}

struct double_complex_alignment {
  char c;
  double complex f;
};

/* initialized in ctypes_double_complex_type_info */
static ffi_type double_complex_ffi_type;

static struct type_info _double_complex_type_info = {
  "double complex",
  UNPASSABLE,
  &double_complex_ffi_type,
  raw_read_double_complex,
  raw_write_double_complex,
};

value ctypes_double_complex_type_info(value unit)
{
  /* It's not clear that the fields we're using here are part of the
     libffi public interface. */
  double_complex_ffi_type.size = sizeof (double complex);
  double_complex_ffi_type.alignment = offsetof
    (struct double_complex_alignment, f);
  double_complex_ffi_type.type = -1;
  double_complex_ffi_type.elements = NULL;

  return ctypes_allocate_type_info(&_double_complex_type_info);
}
