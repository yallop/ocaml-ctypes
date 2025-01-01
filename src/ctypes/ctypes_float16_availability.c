/*
 * Copyright (c) 2025 Ilias Garnier.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 */

#include <caml/mlvalues.h>

#include "ctypes_float16_availability.h"

CAMLprim value ctypes_float16_available(value unit)
{
  return Val_int(FLOAT16_AVAILABLE);
}
