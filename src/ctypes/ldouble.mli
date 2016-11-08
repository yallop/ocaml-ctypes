(*
 * Copyright (c) 2016 Andy Ray.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

type t

val to_float : t -> float 
val of_float : float -> t 

type complex

val real : complex -> t 
val imag : complex -> t 
val complex : t -> t -> complex 

