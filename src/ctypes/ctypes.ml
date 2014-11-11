(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

include Static

include Structs_computed

include Memory

include Std_views

include Coerce

let ( *:* ) s t = field s "<unknown>" t

let ( +:+ ) s t = field s "<unknown>" t
