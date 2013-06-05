(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(** The core ctypes module. 

    The main points of interest are the set of functions for describing C
    types (see {!types}) and the set of functions for accessing C values (see
    {!values}).  The {!foreign} function for binding to native functions is
    the main entry point for the library.
*)

open Signed
open Unsigned

(** {2:types Values representing C types} *)

type 'a typ
(** The type of values representing C types.  There are two types associated
    with each [typ] value: the C type used to store and pass values, and the
    corresponding OCaml type.  The type parameter indicates the OCaml type, so a
    value of type [t typ] is used to read and write OCaml values of type [t].
    There are various uses of [typ] values, including

    - constructing function types for binding native functions using
    {!foreign}

    - constructing pointers for reading and writing locations in C-managed
    storage using {!ptr}

    - describing the fields of structured types built with {!structure} and
    {!union}.
*)

(** {3 The void type} *)

val void  : unit typ
(** Value representing the C void type.  Void values appear in OCaml as the
    unit type, so using void in an argument or result type specification
    produces a function which accepts or returns unit.

    Dereferencing a pointer to void is an error, as in C, and will raise
    [IncompleteType].
*)

(** {3 Scalar types}

    The scalar types consist of the {!arithmetic_types} and the {!pointer_types}.
*)

(** {4:arithmetic_types Arithmetic types} 

    The arithmetic types consist of the signed and unsigned integer types
    (including character types) and the floating types.  There are values
    representing both exact-width integer types (of 8, 16, 32 and 64 bits) and
    types whose size depend on the platform (signed and unsigned short, int, long,
    long long).

*)

val char : char typ
(** Value representing the C type [char]. *)

(** {5 Signed integer types} *)

val schar : int typ
(** Value representing the C type [signed char]. *)

val short : int typ
(** Value representing the C type ([signed]) [short]. *)

val int   : int typ
(** Value representing the C type ([signed]) [int]. *)

val long  : long typ
(** Value representing the C type ([signed]) [long]. *)

val llong  : llong typ
(** Value representing the C type ([signed]) [long long]. *)

val nativeint : nativeint typ
(** Value representing the C type ([signed]) [int]. *)

val int8_t : int typ
(** Value representing an 8-bit signed integer C type. *)

val int16_t : int typ
(** Value representing a 16-bit signed integer C type. *)

val int32_t : int32 typ
(** Value representing a 32-bit signed integer C type. *)

val int64_t : int64 typ
(** Value representing a 64-bit signed integer C type. *)

(** {5 Unsigned integer types} *)

val uchar : uchar typ
(** Value representing the C type [unsigned char]. *)

val uint8_t : uint8 typ
(** Value representing an 8-bit unsigned integer C type. *)

val uint16_t : uint16 typ
(** Value representing a 16-bit unsigned integer C type. *)

val uint32_t : uint32 typ
(** Value representing a 32-bit unsigned integer C type. *)

val uint64_t : uint64 typ
(** Value representing a 64-bit unsigned integer C type. *)

val size_t : size_t typ
(** Value representing the C type [size_t], an alias for one of the unsigned
    integer types.  The actual size and alignment requirements for [size_t]
    vary between platforms. *)

val ushort : ushort typ
(** Value representing the C type [unsigned short]. *)

val uint : uint typ
(** Value representing the C type [unsigned int]. *)

val ulong : ulong typ
(** Value representing the C type [unsigned long]. *)

val ullong : ullong typ
(** Value representing the C type [unsigned long long]. *)

(** {5 Floating types} *)

val float : float typ
(** Value representing the C single-precision [float] type. *)

val double : float typ
(** Value representing the C type [double]. *)

(** {4:pointer_types Pointer types} *)

type 'a ptr
(** The type of pointer values.  A value of type [t ptr] can be used to read
    and write values of type [t] at particular addresses. *)

val ptr : 'a typ -> 'a ptr typ
(** Construct a pointer type from an existing type (called the {i reference
    type}).  *)

val ptr_opt : 'a typ -> 'a ptr option typ
(** Construct a pointer type from an existing type (called the {i reference
    type}).  This behaves like {!ptr}, except that null pointers appear in OCaml
    as [None]. *)

val string : string typ
(** A high-level representation of the string type.

    On the C side this behaves like [char *]; on the OCaml side values read and written using {!string} are simply native OCaml strings.

    To avoid problems with the garbage collector, values passed using {!string} are copied into immovable C-managed storage before being passed to C.
 *)

(** {3 Array types} *)

type 'a array
(** The type of C array values.  A value of type [t array] can be used to read
    and write array objects in C-managed storage. *)

val array : int -> 'a typ -> 'a array typ
(** Construct a sized array type from a length and an existing type (called
    the {i element type}). *)

(** {3 Function types} *)

type 'a fn
(** The type of values representing C function types.  A value of type [t fn]
    can be used to bind to C functions and to describe type of OCaml functions
    passed to C. *)

val ( @-> ) : 'a typ -> 'b fn -> ('a -> 'b) fn
(** Construct a function type from a type and an existing function type.  This
    corresponds to prepending a parameter to a C function parameter list.  For
    example,

    [int @-> ptr void @-> returning float]

    describes a function type that accepts two arguments -- an integer and a
    pointer to void -- and returns a float.
*)

val returning : 'a typ -> 'a fn
(** Give the return type of a C function.  Note that [returning] is intended
    to be used together with {!(@->)}; see the documentation for {!(@->)} for an
    example. *)

val returning_checking_errno : 'a typ -> 'a fn
(** Give the return type of a C function.  This behaves like {!returning},
    except that calls to functions bound using [returning_checking_errno] check
    whether errno has been updated and raise {!Unix.Unix_error} if so. *)

val funptr : ('a -> 'b) fn -> ('a -> 'b) typ
(** Construct a function pointer type from a function type.

    The ctypes library, like C itself, distinguishes functions and function
    pointers.  Functions are not first class: it is not possible to use them
    as arguments or return values of calls, or store them in addressable
    memory.  Function pointers are first class, and so have none of these
    restrictions.
*)

val funptr_opt : ('a -> 'b) fn -> ('a -> 'b) option typ
(** Construct a function pointer type from a function type.

    This behaves like {!funptr}, except that null pointers appear in OCaml as
    [None]. *)

(** {3 Struct and union types} *)

type ('a, 'kind) structured
(** The base type of values representing C struct and union types.  The
    ['kind] parameter is a polymorphic variant type indicating whether the type
    represents a struct ([`Struct]) or a union ([`Union]). *)

type 'a structure = ('a, [`Struct]) structured
(** The type of values representing C struct types. *)

type 'a union = ('a, [`Union]) structured
(** The type of values representing C union types. *)

type ('a, 't) field
(** The type of values representing C struct or union members (called "fields"
    here).  A value of type [(a, s) field] represents a field of type [a] in a
    struct or union of type [s]. *)

val structure : string -> 's structure typ
(** Construct a new structure type.  The type value returned is incomplete and
    can be updated using {!(*:*)} until it is passed to {!seal}, at which point
    the set of fields is fixed.

    The type (['_s structure typ]) of the expression returned by the call
    [structure tag] includes a weak type variable, which can be explicitly
    instantiated to ensure that the OCaml values representing different C
    structure types have incompatible types.  Typical usage is as follows:

    [type tagname]

    [let tagname : tagname structure typ = structure "tagname"]
*)

val ( *:* ) : 's structure typ -> 'a typ -> ('a, 's structure) field
(** [s *:* t] adds a field of type [t] to the structure type [s] and returns
    a field value that can be used to read and write the field in structure
    instances (e.g. using [getf] and [setf]).

    Attempting to add a field to a structure type that has been sealed with
    [seal] is an error, and will raise [ModifyingSealedType]. *)

val union : string -> 's union typ
(** Construct a new union type.  This behaves analogously to {!structure};
    fields are added with {!(+:+)}. *)

val ( +:+ ) : 's union typ -> 'a typ -> ('a, 's union) field
(** [u +:+ t] adds a field of type [t] to the union type [u] and returns a
    field value that can be used to read and write the field in union
    instances (e.g. using [getf] and [setf]).

    Attempting to add a field to a union type that has been sealed with [seal]
    is an error, and will raise [ModifyingSealedType]. *)

val seal : (_, _) structured typ -> unit
(** [seal t] completes the struct or union type [t] so that no further fields
    can be added.  Struct and union types must be sealed before they can be used
    in a way that involves their size or alignment; see the documentation for
    {!IncompleteType} for further details.  *)

(** {3 View types} *)

val view : read:('a -> 'b) -> write:('b -> 'a) -> 'a typ -> 'b typ
(** [view ~read:r ~write:w t] creates a C type representation [t'] which
    behaves like [t] except that values read using [t'] are subsequently
    transformed using the function [r] and values written using [t'] are first
    transformed using the function [w].

    For example, given suitable definitions of [string_of_char_ptr] and
    [char_ptr_of_string], the type representation

    [view ~read:string_of_char_ptr ~write:char_ptr_of_string (ptr char)] 

    can be used to pass OCaml strings directly to and from bound C functions,
    or to read and write string members in structs and arrays.  (In fact, the
    {!string} type representation is defined in exactly this way.)
*)

(** {3 Abstract types} *)

type 'a abstract
(** The type of abstract values.  The purpose of the [abstract] type is to
    represent values whose type varies from platform to platform.

    For example, the type {!Posix.pthread_t} is a pointer on some platforms,
    an integer on other platforms, and a struct on a third set of platforms.
    One way to deal with this kind of situation is to have
    possibly-platform-specific code which interrogates the C type in some way
    to help determine an appropriate representation.  Another way is to use
    [abstract], leaving the representation opaque.

    (Note, however, that although [pthread_t] is a convenient example, since
    the type used to implement it varies significantly across platforms, it's
    not actually a good match for [abstract], since values of type [pthread_t]
    are passed and returned by value.) *)

val abstract : size:int -> alignment:int -> 'a abstract typ
(** Create an abstract type specification from the size and alignment
    requirements for the type. *)

(** {3 Operations on types} *)

val sizeof : 'a typ -> int
(** [sizeof t] computes the size in bytes of the type [t].  The exception
    [IncompleteType] is raised if [t] is incomplete. *)

val alignment : 'a typ -> int
(** [alignment t] computes the alignment requirements of the type [t].  The
    exception [IncompleteType] is raised if [t] is incomplete. *)

(** {2:values Binding C functions and values} *)

val foreign : ?from:Dl.library -> string -> ('a -> 'b) fn -> ('a -> 'b)
(** [foreign name typ] exposes the C function of type [typ] named by [name] as
    an OCaml value.  The argument [?from], if supplied, is a library handle
    returned by {!Dl.dlopen}.  *)

val foreign_value : ?from:Dl.library -> string -> 'a typ -> 'a ptr
(** [foreign_value name typ] exposes the C value of type [typ] named by [name]
    as an OCaml value.  The argument [?from], if supplied, is a library handle
    returned by {!Dl.dlopen}.  *)

(** {3 Pointer values} *)

val null : unit ptr
(** A null pointer. *)

val (!@) : 'a ptr -> 'a
(** [!@ p] dereferences the pointer [p].  If the reference type is a scalar
    type then dereferencing constructs a new value.  If the reference type is
    an aggregate type then dereferencing returns a value that references the
    memory pointed to by [p]. *)

val (<-@) : 'a ptr -> 'a -> unit
(** [p <-@ v] writes the value [v] to the address [p]. *)

val (+@) : 'a ptr -> int -> 'a ptr
(** If [p] is a pointer to an array element then [p +@ n] computes the
    address of the [n]th next element. *)

val (-@) : 'a ptr -> int -> 'a ptr
(** If [p] is a pointer to an array element then [p -@ n] computes the address
    of the nth previous element. *)

val ptr_diff : 'a ptr -> 'a ptr -> int
(** [ptr_diff p q] computes [q - p].  As in C, both [p] and [q] must point
    into the same array, and the result value is the difference of the
    subscripts of the two array elements. *)

val from_voidp : 'a typ -> unit ptr -> 'a ptr
(** Conversion from [void *]. *)

val to_voidp : _ ptr -> unit ptr
(** Conversion to [void *]. *)

val allocate : 'a typ -> 'a -> 'a ptr
(** [allocate t v] allocates a fresh value of type [t], initialises it with [v]
      and returns its address. *)

val allocate_n : 'a typ -> count:int -> 'a ptr
(** [allocate_n t ~count:n] allocates a fresh array with element type [t]
    and length [n], and returns its address. *)

val ptr_compare : 'a ptr -> 'a ptr -> int
(** If [p] and [q] are pointers to elements [i] and [j] of the same array then
    [ptr_compare p q] compares the indexes of the elements.  The result is
    negative if [i] is less than [j], positive if [i] is greater than [j], and
    zero if [i] and [j] are equal. *)

val ptr_of_raw_address : int64 -> unit ptr
(** Convert the numeric representation of an address to a pointer *)

(** {3 Array values} *)

module Array :
sig
  type 'a t = 'a array

  val get : 'a t -> int -> 'a
  (** [get a n] returns the [n]th element of the zero-indexed array [a].  The
      semantics for non-scalar types are non-copying, as for {!(!@)}.

      You can also write [a.(n)] instead of [Array.get a n].

      Raise [Invalid_argument "index out of bounds"] if [n] is outside of the
      range [0] to [(Array.length a - 1)]. *)

  val set : 'a t -> int -> 'a -> unit
  (** [set a n v] overwrites the [n]th element of the zero-indexed array [a]
      with [v].

      You can also write [a.(n) <- v] instead of [Array.set a n v].

      Raise [Invalid_argument "index out of bounds"] if [n] is outside of the range [0] to [(Array.length a - 1)]. *)

  val unsafe_get : 'a t -> int -> 'a
  (** [unsafe_get a n] behaves like [get a n] except that the check that [n]
      between [0] and [(Array.length a - 1)] is not performed. *)

  val unsafe_set : 'a t -> int -> 'a -> unit
  (** [unsafe_set a n v] behaves like [set a n v] except that the check that
      [n] between [0] and [(Array.length a - 1)] is not performed. *)

  val of_list : 'a typ -> 'a list -> 'a t
  (** [of_list t l] builds an array of type [t] of the same length as [l], and
      writes the elements of [l] to the corresponding elements of the array. *)

  val to_list : 'a t -> 'a list
  (** [to_list a] builds a list of the same length as [a] such that each
      element of the list is the result of reading the corresponding element of
      [a]. *)

  val length : 'a t -> int
  (** Return the number of elements of the given array. *)

  val start : 'a t -> 'a ptr
  (** Return the address of the first element of the given array. *)

  val from_ptr : 'a ptr -> int -> 'a t
  (** [from_ptr p n] creates an [n]-length array reference to the memory at
      address [p]. *)

  val make : 'a typ -> ?initial:'a -> int -> 'a t
(** [make t n] creates an [n]-length array of type [t].  If the optional
    argument [?initial] is supplied, it indicates a value that should be used to
    initialise every element of the array.  *)
end
(** Operations on C arrays. *)

(** {3 Struct and union values} *)

val make : ((_, _) structured as 's) typ -> 's
(** Allocate a fresh, uninitialised structure or union value. *)

val setf : ((_, _) structured as 's) -> ('a, 's) field -> 'a -> unit
(** [setf s f v] overwrites the value of the field [f] in the structure or
    union [s] with [v]. *)

val getf : ((_, _) structured as 's) -> ('a, 's) field -> 'a
(** [getf s f] retrieves the value of the field [f] in the structure or union [s].  The
      semantics for non-scalar types are non-copying, as for {!(!@)}.*)

val (@.) : ((_, _) structured as 's) -> ('a, 's) field -> 'a ptr
(** [s @. f] computes the address of the field [f] in the structure or union
    value [s]. *)

val (|->) : ((_, _) structured as 's) ptr -> ('a, 's) field -> 'a ptr
(** [p |-> f] computes the address of the field [f] in the structure or union
    value pointed to by [p]. *)

val offsetof : (_, (_, [`Struct]) structured) field -> int
(** [offsetof f] returns the offset, in bytes, of the field [f] from the
    beginning of the associated struct type. *)

val addr : ((_, _) structured as 's) -> 's ptr
(** [addr s] returns the address of the structure or union [s]. *)

(** {2 Exceptions} *)

exception Unsupported of string
(** An attempt was made to use a feature not currently supported by ctypes.
    In practice this refers to attempts to use an union, array or abstract
    type as an argument or return type of a function. *)

exception ModifyingSealedType of string
(** An attempt was made to modify a sealed struct or union type
    description.  *)

exception IncompleteType
(** An attempt was made to compute the size or alignment of an incomplete
    type.

    The incomplete types are struct and union types that have not been sealed,
    and the void type.

    It is not permitted to compute the size or alignment requirements of an
    incomplete type, to use it as a struct or union member, to read or write a
    value of the type through a pointer or to use it as the referenced type in
    pointer arithmetic.  Additionally, incomplete struct and union types
    cannot be used as argument or return types.
*)
