type 'a typ
type 'a fn
type 'a ptr
type 'a array
type 'a structure
type 'a union
type 'a abstract

exception Unsupported of string
exception ModifyingSealedType of string
exception IncompleteType

module Ptr :
sig
  type 'a t = 'a ptr

  val null : unit ptr
  val (!) : 'a t -> 'a
  val (:=) : 'a t -> 'a -> unit
  val (+) : 'a t -> int -> 'a t
  val (-) : 'a t -> int -> 'a t
  val diff : 'a t -> 'a t -> int
  val from_voidp : 'a typ -> unit ptr -> 'a ptr
  val to_voidp : _ ptr -> unit ptr
  val make : 'a typ -> 'a -> 'a ptr
  val allocate : 'a typ -> count:int -> 'a ptr
end

module Array :
sig
  type 'a t = 'a array

  val get : 'a t -> int -> 'a
  val set : 'a t -> int -> 'a -> unit
  val unsafe_get : 'a t -> int -> 'a
  val unsafe_set : 'a t -> int -> 'a -> unit
  val of_list : 'a typ -> 'a list -> 'a t
  val to_list : 'a t -> 'a list
  val length : 'a t -> int
  val start : 'a t -> 'a ptr
  val from_ptr : 'a ptr -> int -> 'a t
  val make : 'a typ -> ?initial:'a -> int -> 'a t
end

module Struct :
sig
  type 's t = 's structure
  type ('a, -'s) field

  val structure : string -> 's structure typ
  val ( *:* ) : 's structure typ -> 'a typ -> ('a, 's) field
  val seals : 's structure typ -> unit

  val make : 's structure typ -> 's structure
  val setf : 's structure -> ('a, 's) field -> 'a -> unit
  val getf : 's structure -> ('a, 's) field -> 'a
  val (@.) : 's structure -> ('a, 's) field -> 'a ptr
  val (|->) : 's structure ptr -> ('a, 's) field -> 'a ptr
  val offsetof : (_, _) field -> int
  val addr : 's structure -> 's structure ptr
end

module Union :
sig
  type 's t = 's union
  type ('a, -'s) field

  val union : string -> 's union typ
  val ( +:+ ) : 's union typ -> 'a typ -> ('a, 's) field
  val sealu : 's union typ -> unit

  val make : 's union typ -> 's union
  val setf : 's union -> ('a, 's) field -> 'a -> unit
  val getf : 's union -> ('a, 's) field -> 'a
  val (@.) : 's union -> ('a, 's) field -> 'a ptr
  val (|->) : 's union ptr -> ('a, 's) field -> 'a ptr
  val addr : 's union -> 's union ptr
end

val sizeof : 'a typ -> int
val alignment : 'a typ -> int

open Signed
open Unsigned

val void  : unit typ
val char : char typ
val schar : int typ
val float : float typ
val double : float typ
val short : int typ
val int   : int typ
val long  : long typ
val llong  : llong typ
val nativeint : nativeint typ
val int8_t : int typ
val int16_t : int typ
val int32_t : int32 typ
val int64_t : int64 typ
val uchar : uchar typ
val uint8_t : uint8 typ
val uint16_t : uint16 typ
val uint32_t : uint32 typ
val uint64_t : uint64 typ
val size_t : size_t typ
val ushort : ushort typ
val uint : uint typ
val ulong : ulong typ
val ullong : ullong typ

val string : string typ

val view : read:('a -> 'b) -> write:('b -> 'a) -> 'a typ -> 'b typ
val abstract : size:int -> alignment:int -> 'a abstract typ
val array : int -> 'a typ -> 'a array typ
val ptr : 'a typ -> 'a ptr typ
val ( @-> ) : 'a typ -> 'b fn -> ('a -> 'b) fn

val returning : 'a typ -> 'a fn
val syscall : 'a typ -> 'a fn
val funptr : ('a -> 'b) fn -> ('a -> 'b) typ

val foreign : ?from:Dl.library -> string -> ('a -> 'b) fn -> ('a -> 'b)
val foreign_value : ?from:Dl.library -> string -> 'a typ -> 'a ptr
