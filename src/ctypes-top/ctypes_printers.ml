(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

let format_typ fmt t = Ctypes.format_typ fmt t
let format_fn fmt fn = Ctypes.format_fn fmt fn
let format_pointer fmt v =
  let open Ctypes in
  let typ = ptr (reference_type v) in
  Format.fprintf fmt "(%a) %a" (fun fmt -> format_typ fmt) typ (format typ) v
let format_struct fmt v =
  Ctypes.(format (reference_type (addr v)) fmt v)
let format_union fmt v =
  Ctypes.(format (reference_type (addr v)) fmt v)
let format_array fmt v =
  Ctypes.(format CArray.(array (length v) (reference_type (start v))) fmt v)
