open PosixTypes
open Ffi
open Type

type t = sigset_t ptr

val t : sigset_t ptr typ

val empty : unit -> t

val full : unit -> t

val add : t -> int -> unit

val del : t -> int -> unit

val mem : t -> int -> bool
