/*
 * Copyright (c) 2025 Ilias Garnier.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 */


#ifndef CTYPES_FLOAT16_AVAILABILITY_H
#define CTYPES_FLOAT16_AVAILABILITY_H

#define __STDC_WANT_IEC_60559_TYPES_EXT__
#include <float.h>

#include <caml/version.h>

/* According to the annex X.3 of the C11 standard, if __STDC_WANT_IEC_60559_TYPES_EXT__
 * is defined before including <float.h>, then FLT16_MAX and FLT16_MIN are defined if _Float16
 * is available.
 */
#ifdef FLT16_MAX
#ifdef FLT16_MIN
#define FLOAT16_AVAILABLE 1
#else
#define FLOAT16_AVAILABLE 0
#endif
#else
#define FLOAT16_AVAILABLE 0
#endif

#define FLOAT16_BIGARRAY_AVAILABLE (OCAML_VERSION_MAJOR > 5 || (OCAML_VERSION_MAJOR == 5 && OCAML_VERSION_MINOR >= 2))

/* float16_available : unit -> bool */
extern value ctypes_float16_available(value unit);

#endif /* CTYPES_FLOAT16_AVAILABILITY_H */
