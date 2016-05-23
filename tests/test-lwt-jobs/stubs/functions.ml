(*
 * Copyright (c) 2016 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Foreign function bindings for the Lwt jobs tests. *)

open Ctypes

module Stubs (F: Cstubs.FOREIGN) =
struct
  open F

  let sqrt = foreign "sqrt" (double @-> returning double)

  let sum_int_array = foreign "sum_int_array"
      (ptr int32_t @-> size_t @-> returning int32_t)
end
