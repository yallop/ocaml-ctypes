(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

include Static

include Structs_computed

include Type_printing

include Memory

include Std_views

include Value_printing

include Coerce

let ( *:* ) s t = field s "<unknown>" t

let ( +:+ ) s t = field s "<unknown>" t

module CArray = CArray

let bigarray_of_array = CArray.to_bigarray
let array_of_bigarray = CArray.of_bigarray
