(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Foreign function bindings for the union tests. *)

open Ctypes
open Tests_common
open Types

module Stubs (F: FOREIGN) =
struct
  open F

  let sum_union_components = foreign "sum_union_components"
    (ptr padded @-> size_t @-> returning int64_t)
end
