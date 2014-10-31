(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Ctypes

type computed = Computed and immediate = Immediate

module type FUTURE =
sig
  type (_, _) future

  val (!^) : 'a -> (computed, 'a) future
  val (<*>) : (_, 'a -> 'b) future -> (_, 'a) future -> (computed, 'b) future
end

module type STRUCT =
sig
  include FUTURE

  val structure : string -> (immediate, 's structure typ) future
  val union : string -> (immediate, 's union typ) future

  val field : (immediate, 't typ) future -> string -> (_, 'a typ) future ->
    (computed, ('a, (('s, [<`Struct | `Union]) structured as 't)) field) future

  val seal : (immediate, (_, [< `Struct | `Union]) structured typ) future -> unit
end

module type BINDINGS = functor (F : STRUCT) -> sig end

val write_c : Format.formatter -> (module BINDINGS) -> unit
