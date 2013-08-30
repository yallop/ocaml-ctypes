/*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 */

#include <caml/mlvalues.h>
#include <caml/bigarray.h>

#include "raw_pointer.h"

/* address : 'b -> pointer */
value ctypes_bigarray_address(value ba)
{
  return CTYPES_FROM_PTR(Caml_ba_data_val(ba));
}

/* _view : ('a, 'b) kind -> dims:int array -> ptr -> offset:int ->
           ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t */
value ctypes_bigarray_view(value kind_, value dims_, value ptr_, value offset_)
{
  int kind = Int_val(kind_);
  int ndims = Wosize_val(dims_);
  int offset = Int_val(offset_);
  intnat dims[CAML_BA_MAX_NUM_DIMS];
  int i;
  for (i = 0; i < ndims; i++) {
    dims[i] = Int_val(Field(dims_, i));
  }
  int flags = kind | CAML_BA_C_LAYOUT | CAML_BA_EXTERNAL;
  void *data = offset + (char *)CTYPES_TO_PTR(ptr_);
  return caml_ba_alloc(flags, ndims, data, dims);
}
