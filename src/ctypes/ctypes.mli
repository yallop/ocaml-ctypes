(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(** The core ctypes module.

    The main points of interest are the set of functions for describing C
    types (see {!types}) and the set of functions for accessing C values (see
    {!values}).  The {!Foreign.foreign} function uses C type descriptions
    to bind external C values.
*)

(* open Signed *)
(* open Unsigned *)

include Ctypes_types.TYPES
 with type 'a ptr = 'a Ctypes_types.Types.ptr
include Ctypes_types.TYP
 with type ('a, 's) field := ('a, 's) field
 and type 'a typ = 'a Static.typ
include Ctypes_types.FUNCTION

(** {3 Operations on types} *)

val sizeof : 'a typ -> int
(** [sizeof t] computes the size in bytes of the type [t].  The exception
    {!IncompleteType} is raised if [t] is incomplete. *)

val alignment : 'a typ -> int
(** [alignment t] computes the alignment requirements of the type [t].  The
    exception {!IncompleteType} is raised if [t] is incomplete. *)

val format_typ : ?name:string -> Format.formatter -> 'a typ -> unit
(** Pretty-print a C representation of the type to the specified formatter. *)

val format_fn : ?name:string -> Format.formatter -> 'a fn -> unit
(** Pretty-print a C representation of the function type to the specified
    formatter. *)

val string_of_typ : ?name:string -> 'a typ -> string
(** Return a C representation of the type. *)

val string_of_fn : ?name:string -> 'a fn -> string
(** Return a C representation of the function type. *)

(** {2:values Values representing C values} *)

val format : 'a typ -> Format.formatter -> 'a -> unit
(** Pretty-print a representation of the C value to the specified formatter. *)

val string_of : 'a typ -> 'a -> string
(** Return a string representation of the C value. *)

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

val (+@) : ('a, 'b) pointer -> int -> ('a, 'b) pointer
(** If [p] is a pointer to an array element then [p +@ n] computes the
    address of the [n]th next element. *)

val (-@) : ('a, 'b) pointer -> int -> ('a, 'b) pointer
(** If [p] is a pointer to an array element then [p -@ n] computes the address
    of the nth previous element. *)

val ptr_diff : ('a, 'b) pointer -> ('a, 'b) pointer -> int
(** [ptr_diff p q] computes [q - p].  As in C, both [p] and [q] must point
    into the same array, and the result value is the difference of the
    subscripts of the two array elements. *)

val from_voidp : 'a typ -> unit ptr -> 'a ptr
(** Conversion from [void *]. *)

val to_voidp : _ ptr -> unit ptr
(** Conversion to [void *]. *)

val allocate : ?finalise:('a ptr -> unit) -> 'a typ -> 'a -> 'a ptr
(** [allocate t v] allocates a fresh value of type [t], initialises it with
    [v] and returns its address.  The argument [?finalise], if present, will be
    called just before the memory is freed.  *)

val allocate_n : ?finalise:('a ptr -> unit) -> 'a typ -> count:int -> 'a ptr
(** [allocate_n t ~count:n] allocates a fresh array with element type [t] and
    length [n], and returns its address.  The argument [?finalise], if present,
    will be called just before the memory is freed.  *)

val ptr_compare : 'a ptr -> 'a ptr -> int
(** If [p] and [q] are pointers to elements [i] and [j] of the same array then
    [ptr_compare p q] compares the indexes of the elements.  The result is
    negative if [i] is less than [j], positive if [i] is greater than [j], and
    zero if [i] and [j] are equal. *)

val reference_type : 'a ptr -> 'a typ
(** Retrieve the reference type of a pointer. *)

val ptr_of_raw_address : nativeint -> unit ptr
(** Convert the numeric representation of an address to a pointer *)

val raw_address_of_ptr : unit ptr -> nativeint
(** [raw_address_of_ptr p] returns the numeric representation of p.

    Note that the return value remains valid only as long as the pointed-to
    object is alive.  If [p] is a managed object (e.g. a value returned by
    {!make}) then unless the caller retains a reference to [p], the object may
    be collected, invalidating the returned address. *)

val string_from_ptr : char ptr -> length:int -> string
(** [string_from_ptr p ~length] creates a string initialized with the [length]
    characters at address [p].

    Raise [Invalid_argument "Ctypes.string_from_ptr"] if [length] is
    negative. *)

val ocaml_string_start : string -> string ocaml
(** [ocaml_string_start s] allows to pass a pointer to the contents of an OCaml
    string directly to a C function. *)

val ocaml_bytes_start : Bytes.t -> Bytes.t ocaml
(** [ocaml_bytes_start s] allows to pass a pointer to the contents of an OCaml
    byte array directly to a C function. *)

(** {3 Array values} *)

(** {4 C array values} *)

module CArray :
sig
  type 'a t = 'a carray

  val get : 'a t -> int -> 'a
  (** [get a n] returns the [n]th element of the zero-indexed array [a].  The
      semantics for non-scalar types are non-copying, as for {!(!@)}.

      If you rebind the [CArray] module to [Array] then you can also use the
      syntax [a.(n)] instead of [Array.get a n].

      Raise [Invalid_argument "index out of bounds"] if [n] is outside of the
      range [0] to [(CArray.length a - 1)]. *)

  val set : 'a t -> int -> 'a -> unit
  (** [set a n v] overwrites the [n]th element of the zero-indexed array [a]
      with [v].

      If you rebind the [CArray] module to [Array] then you can also use the
      [a.(n) <- v] syntax instead of [Array.set a n v].

      Raise [Invalid_argument "index out of bounds"] if [n] is outside of the
      range [0] to [(CArray.length a - 1)]. *)

  val unsafe_get : 'a t -> int -> 'a
  (** [unsafe_get a n] behaves like [get a n] except that the check that [n]
      between [0] and [(CArray.length a - 1)] is not performed. *)

  val unsafe_set : 'a t -> int -> 'a -> unit
  (** [unsafe_set a n v] behaves like [set a n v] except that the check that
      [n] between [0] and [(CArray.length a - 1)] is not performed. *)

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

  val make : ?finalise:('a t -> unit) -> 'a typ -> ?initial:'a -> int -> 'a t
  (** [make t n] creates an [n]-length array of type [t].  If the optional
      argument [?initial] is supplied, it indicates a value that should be
      used to initialise every element of the array.  The argument [?finalise],
      if present, will be called just before the memory is freed. *)

  val element_type : 'a t -> 'a typ
(** Retrieve the element type of an array. *)
end
(** Operations on C arrays. *)

(** {4 Bigarray values} *)

val bigarray_start : < element: 'a;
                       ba_repr: _;
                       bigarray: 'b;
                       carray: _;
                       dims: _ > bigarray_class -> 'b -> 'a ptr
(** Return the address of the first element of the given Bigarray value. *)

val bigarray_of_ptr : < element: 'a;
                        ba_repr: 'f;
                        bigarray: 'b;
                        carray: _;
                        dims: 'i > bigarray_class ->
    'i -> ('a, 'f) Bigarray.kind -> 'a ptr -> 'b
(** [bigarray_of_ptr c dims k p] converts the C pointer [p] to a bigarray
    value.  No copy is made; the bigarray references the memory pointed to by
    [p]. *)

val array_of_bigarray : < element: _;
                          ba_repr: _;
                          bigarray: 'b;
                          carray: 'c;
                          dims: _ > bigarray_class -> 'b -> 'c
(** [array_of_bigarray c b] converts the bigarray value [b] to a value of type
    {!CArray.t}.  No copy is made; the result occupies the same memory as
    [b]. *)

(** Convert a Bigarray value to a C array. *)

val bigarray_of_array : < element: 'a;
                          ba_repr: 'f;
                          bigarray: 'b;
                          carray: 'c carray;
                          dims: 'i > bigarray_class ->
    ('a, 'f) Bigarray.kind -> 'c carray -> 'b
(** [bigarray_of_array c k a] converts the {!CArray.t} value [c] to a bigarray
    value.  No copy is made; the result occupies the same memory as [c]. *)

(** {3 Struct and union values} *)

val make : ?finalise:('s -> unit) -> ((_, _) structured as 's) typ -> 's
(** Allocate a fresh, uninitialised structure or union value.  The argument
    [?finalise], if present, will be called just before the underlying memory is
    freed. *)

val setf : ((_, _) structured as 's) -> ('a, 's) field -> 'a -> unit
(** [setf s f v] overwrites the value of the field [f] in the structure or
    union [s] with [v]. *)

val getf : ((_, _) structured as 's) -> ('a, 's) field -> 'a
(** [getf s f] retrieves the value of the field [f] in the structure or union
    [s].  The semantics for non-scalar types are non-copying, as for
    {!(!@)}.*)

val (@.) : ((_, _) structured as 's) -> ('a, 's) field -> 'a ptr
(** [s @. f] computes the address of the field [f] in the structure or union
    value [s]. *)

val (|->) : ((_, _) structured as 's) ptr -> ('a, 's) field -> 'a ptr
(** [p |-> f] computes the address of the field [f] in the structure or union
    value pointed to by [p]. *)

val offsetof : (_, _ structure) field -> int
(** [offsetof f] returns the offset, in bytes, of the field [f] from the
    beginning of the associated struct type. *)

val field_type : ('a, _) field -> 'a typ
(** [field_type f] returns the type of the field [f]. *)

val field_name : (_, _) field -> string
(** [field_name f] returns the name of the field [f]. *)

val addr : ((_, _) structured as 's) -> 's ptr
(** [addr s] returns the address of the structure or union [s]. *)

(** {3 Coercions} *)

val coerce : 'a typ -> 'b typ -> 'a -> 'b
(** [coerce t1 t2] returns a coercion function between the types represented
    by [t1] and [t2].  If [t1] cannot be coerced to [t2], [coerce] raises
    {!Uncoercible}.

    The following coercions are currently supported:

     - All pointer types are intercoercible.
     - Any type may be coerced to {!void}
     - There is a coercion between a {!view} and another type [t] (in either
       direction) if there is a coercion between the representation type
       underlying the view and [t].
     - Coercion is transitive: if [t1] is coercible to [t2] and [t2] is
       coercible to [t3], then [t1] is directly coercible to [t3].

    The set of supported coercions is subject to change.  Future versions of
    ctypes may both add new types of coercion and restrict the existing
    coercions. *)

val coerce_fn : 'a fn -> 'b fn -> 'a -> 'b
(** [coerce_fn f1 f2] returns a coercion function between the function
    types represented by [f1] and [f2].  If [f1] cannot be coerced to
    [f2], [coerce_fn] raises {!Uncoercible}.

    A function type [f1] may be coerced to another function type [f2]
    if all of the following hold:

      - the C types described by [f1] and [f2] have the same arity

      - each argument of [f2] may be coerced to the corresponding
        argument of [f1]

      - the return type of [f1] may be coerced to the return type of [f2]

    The set of supported coercions is subject to change.  Future versions of
    ctypes may both add new types of coercion and restrict the existing
    coercions. *)

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

exception Uncoercible
(** An attempt was made to coerce between uncoercible types.  *)
