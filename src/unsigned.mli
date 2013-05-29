module type S = sig
  type t

  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val rem : t -> t -> t
  val max_int : t
  val logand : t -> t -> t
  val logor : t -> t -> t
  val logxor : t -> t -> t
  val shift_left : t -> int -> t
  val shift_right : t -> int -> t
  val of_int : int -> t
  val to_int : t -> int
  val of_string : string -> t
  val to_string : t -> string
  val zero : t
  val one : t
  val lognot : t -> t
  val succ : t -> t
  val pred : t -> t
  val compare : t -> t -> int

  module Infix : sig
    val (+) : t -> t -> t
    val (-) : t -> t -> t
    val ( * ) : t -> t -> t
    val (/) : t -> t -> t
    val (mod) : t -> t -> t
    val (land) : t -> t -> t
    val (lor) : t -> t -> t
    val (lxor) : t -> t -> t
    val (lsl) : t -> int -> t
    val (lsr) : t -> int -> t
  end
end

module UChar : S
module UInt8 : S
module UInt16 : S
module UInt32 : S
module UInt64 : S
module Size_t : S
module UShort : S
module UInt : S
module ULong : S
module ULLong : S

type uchar = UChar.t
type uint8 = UInt8.t
type uint16 = UInt16.t
type uint32 = UInt32.t
type uint64 = UInt64.t
type size_t = Size_t.t
type ushort = UShort.t
type uint = UInt.t
type ulong = ULong.t
type ullong = ULLong.t
