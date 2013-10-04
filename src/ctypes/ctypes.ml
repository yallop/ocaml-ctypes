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

let ( *:* ) s t =
  Common.warn "'s *:* t' is deprecated; use 'field s label t' instead";
  field s "<unknown>" t

let ( +:+ ) s t =
  Common.warn "'s +:+ t' is deprecated; use 'field s label t' instead";
  field s "<unknown>" t
