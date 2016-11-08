(*
 * Copyright (c) 2016 Andy Ray.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

type t

external to_float : t -> float = "ctypes_ldouble_to_float"
external of_float : float -> t = "ctypes_ldouble_of_float"

type complex

external real : complex -> t = "ctypes_ldouble_complex_real"
external imag : complex -> t = "ctypes_ldouble_complex_imag"
external complex : t -> t -> complex = "ctypes_ldouble_complex_make"

