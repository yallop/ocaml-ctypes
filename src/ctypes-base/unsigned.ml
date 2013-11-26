(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Boxed unsigned types *)
module type Basics = sig
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
end


module type Extras = sig
  type t

  val zero : t
  val one : t
  val lognot : t -> t
  val succ : t -> t
  val pred : t -> t
  val compare : t -> t -> int
end


module type Infix = sig
  type t
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


module type S = sig
  include Basics
  include Extras with type t := t

  module Infix : Infix with type t := t
end


module MakeInfix (B : Basics) =
struct
  open B
  let (+) = add
  let (-) = sub
  let ( * ) = mul
  let (/) = div
  let (mod) = rem
  let (land) = logand
  let (lor) = logor
  let (lxor) = logxor
  let (lsl) = shift_left
  let (lsr) = shift_right
end


module Extras(Basics : Basics) : Extras with type t := Basics.t =
struct
  open Basics
  let zero = of_int 0
  let one = of_int 1
  let succ n = add n one
  let pred n = sub n one
  let lognot n = logxor n max_int
  let compare (x : t) (y : t) = Pervasives.compare x y
end


module UInt8 : S = 
struct
  module B =
  struct
    type t
    external add : t -> t -> t = "ctypes_uint8_add"
    external sub : t -> t -> t = "ctypes_uint8_sub"
    external mul : t -> t -> t = "ctypes_uint8_mul"
    external div : t -> t -> t = "ctypes_uint8_div"
    external rem : t -> t -> t = "ctypes_uint8_rem"
    external logand : t -> t -> t = "ctypes_uint8_logand"
    external logor : t -> t -> t = "ctypes_uint8_logor"
    external logxor : t -> t -> t = "ctypes_uint8_logxor"
    external shift_left : t -> int -> t = "ctypes_uint8_shift_left"
    external shift_right : t -> int -> t = "ctypes_uint8_shift_right"
    external of_int : int -> t = "ctypes_uint8_of_int"
    external to_int : t -> int = "ctypes_uint8_to_int"
    external of_string : string -> t = "ctypes_uint8_of_string"
    external to_string : t -> string = "ctypes_uint8_to_string"
    external _max_int : unit -> t = "ctypes_uint8_max"
    let max_int = _max_int ()
  end
  include B
  include Extras(B)
  module Infix = MakeInfix(B)
end


module UInt16 : S =
struct
  module B =
  struct
    type t
    external add : t -> t -> t = "ctypes_uint16_add"
    external sub : t -> t -> t = "ctypes_uint16_sub"
    external mul : t -> t -> t = "ctypes_uint16_mul"
    external div : t -> t -> t = "ctypes_uint16_div"
    external rem : t -> t -> t = "ctypes_uint16_rem"
    external logand : t -> t -> t = "ctypes_uint16_logand"
    external logor : t -> t -> t = "ctypes_uint16_logor"
    external logxor : t -> t -> t = "ctypes_uint16_logxor"
    external shift_left : t -> int -> t = "ctypes_uint16_shift_left"
    external shift_right : t -> int -> t = "ctypes_uint16_shift_right"
    external of_int : int -> t = "ctypes_uint16_of_int"
    external to_int : t -> int = "ctypes_uint16_to_int"
    external of_string : string -> t = "ctypes_uint16_of_string"
    external to_string : t -> string = "ctypes_uint16_to_string"
    external _max_int : unit -> t = "ctypes_uint16_max"
    let max_int = _max_int ()
  end
  include B
  include Extras(B)
  module Infix = MakeInfix(B)
end


module UInt32 : sig
  include S
  external of_int32 : int32 -> t = "ctypes_uint32_of_int32"
  external to_int32 : t -> int32 = "ctypes_int32_of_uint32"
end = 
struct
  module B =
  struct
    type t
    external add : t -> t -> t = "ctypes_uint32_add"
    external sub : t -> t -> t = "ctypes_uint32_sub"
    external mul : t -> t -> t = "ctypes_uint32_mul"
    external div : t -> t -> t = "ctypes_uint32_div"
    external rem : t -> t -> t = "ctypes_uint32_rem"
    external logand : t -> t -> t = "ctypes_uint32_logand"
    external logor : t -> t -> t = "ctypes_uint32_logor"
    external logxor : t -> t -> t = "ctypes_uint32_logxor"
    external shift_left : t -> int -> t = "ctypes_uint32_shift_left"
    external shift_right : t -> int -> t = "ctypes_uint32_shift_right"
    external of_int : int -> t = "ctypes_uint32_of_int"
    external to_int : t -> int = "ctypes_uint32_to_int"
    external of_string : string -> t = "ctypes_uint32_of_string"
    external to_string : t -> string = "ctypes_uint32_to_string"
    external _max_int : unit -> t = "ctypes_uint32_max"
    let max_int = _max_int ()
  end
  include B
  include Extras(B)
  module Infix = MakeInfix(B)
  external of_int32 : int32 -> t = "ctypes_uint32_of_int32"
  external to_int32 : t -> int32 = "ctypes_int32_of_uint32"
end


module UInt64 : sig
  include S
  external of_int64 : int64 -> t = "ctypes_uint64_of_int64"
  external to_int64 : t -> int64 = "ctypes_int64_of_uint64"
end = 
struct
  module B =
  struct
    type t
    external add : t -> t -> t = "ctypes_uint64_add"
    external sub : t -> t -> t = "ctypes_uint64_sub"
    external mul : t -> t -> t = "ctypes_uint64_mul"
    external div : t -> t -> t = "ctypes_uint64_div"
    external rem : t -> t -> t = "ctypes_uint64_rem"
    external logand : t -> t -> t = "ctypes_uint64_logand"
    external logor : t -> t -> t = "ctypes_uint64_logor"
    external logxor : t -> t -> t = "ctypes_uint64_logxor"
    external shift_left : t -> int -> t = "ctypes_uint64_shift_left"
    external shift_right : t -> int -> t = "ctypes_uint64_shift_right"
    external of_int : int -> t = "ctypes_uint64_of_int"
    external to_int : t -> int = "ctypes_uint64_to_int"
    external of_string : string -> t = "ctypes_uint64_of_string"
    external to_string : t -> string = "ctypes_uint64_to_string"
    external _max_int : unit -> t = "ctypes_uint64_max"
    let max_int = _max_int ()
  end
  include B
  include Extras(B)
  module Infix = MakeInfix(B)
  external of_int64 : int64 -> t = "ctypes_uint64_of_int64"
  external to_int64 : t -> int64 = "ctypes_int64_of_uint64"
end


let pick : size:int -> (module S) =
  fun ~size -> match size with
    | 1 -> (module UInt8)
    | 2 -> (module UInt16)
    | 4 -> (module UInt32)
    | 8 -> (module UInt64)
    | _ -> assert false
      
external size_t_size : unit -> int = "ctypes_size_t_size"
external ushort_size : unit -> int = "ctypes_ushort_size"
external uint_size : unit -> int = "ctypes_uint_size"
external ulong_size : unit -> int = "ctypes_ulong_size"
external ulonglong_size : unit -> int = "ctypes_ulonglong_size"

module Size_t : S = (val pick ~size:(size_t_size ()))
module UChar : S = UInt8
module UShort : S = (val pick ~size:(ushort_size ()))
module UInt : S = (val pick ~size:(uint_size ()))
module ULong : S = (val pick ~size:(ulong_size ()))
module ULLong : S = (val pick ~size:(ulonglong_size ()))

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
