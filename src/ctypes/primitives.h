/*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 */

#ifndef CTYPES_PRIMITIVES_H
#define CTYPES_PRIMITIVES_H

#include <limits.h>
#include <stdint.h>

/* The order here must correspond to the constructor order in primitives.ml */
enum ctypes_primitive {
  Char,
  Schar,
  Short,
  Int,
  Int8_t,
  Int16_t,
  Int32_t,
  Int64_t,
  Camlint,
  Nativeint,
  Float,
  Double,
};

#endif /* CTYPES_PRIMITIVES_H */
