(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Ctypes
open PosixTypes
open Date_type

module Bindings (F : sig val foreign : string -> ('a -> 'b) fn -> unit end) =
struct
  open F

  let time = foreign "time" (ptr time_t @-> returning time_t)

  let asctime = foreign "asctime" (ptr tm @-> returning string)

  let localtime = foreign "localtime" (ptr time_t @-> returning (ptr tm))
end
