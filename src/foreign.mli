(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(** High-level bindings for C functions and values *)

val foreign : ?from:Dl.library -> string -> ('a -> 'b) Ctypes.fn -> ('a -> 'b)
(** [foreign name typ] exposes the C function of type [typ] named by [name] as
    an OCaml value.  The argument [?from], if supplied, is a library handle
    returned by {!Dl.dlopen}.  *)

val foreign_value : ?from:Dl.library -> string -> 'a Ctypes.typ -> 'a Ctypes.ptr
(** [foreign_value name typ] exposes the C value of type [typ] named by [name]
    as an OCaml value.  The argument [?from], if supplied, is a library handle
    returned by {!Dl.dlopen}.  *)
