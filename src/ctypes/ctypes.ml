(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

include Ctypes_static

include Ctypes_structs_computed

include Ctypes_type_printing

include Ctypes_memory

include Ctypes_std_views

include Ctypes_value_printing

include Ctypes_coerce

let lift_typ x = x
