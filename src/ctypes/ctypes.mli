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

open Signed
open Unsigned

(** {2:types Values representing C types} *)

type 'a typ = 'a Static.typ
(** The type of values representing C types.  There are two types associated
    with each [typ] value: the C type used to store and pass values, and the
    corresponding OCaml type.  The type parameter indicates the OCaml type, so a
    value of type [t typ] is used to read and write OCaml values of type [t].
    There are various uses of [typ] values, including

    - constructing function types for binding native functions using
    {!Foreign.foreign}

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
    {!IncompleteType}.
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

val camlint : int typ
(** Value representing an integer type with the same storage requirements as
    an OCaml [int]. *)

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

(** {5 Complex types} *)

val complex32 : Complex.t typ
(** Value representing the C99 single-precision [float complex] type. *)

val complex64 : Complex.t typ
(** Value representing the C99 double-precision [double complex] type. *)

(** {4:pointer_types Pointer types} *)

type ('a, 'b) pointer = ('a, 'b) Static.pointer
(** The type of pointer values. A value of type [('a, [`C]) pointer] contains
    a C-compatible pointer, and a value of type [('a, [`OCaml]) pointer]
    contains a pointer to a value that can be moved by OCaml runtime. *)

(** {4 C-compatible pointers} *)

type 'a ptr = ('a, [`C]) pointer
(** The type of C-compatible pointer values.  A value of type [t ptr] can be
    used to read and write values of type [t] at particular addresses. *)

val ptr : 'a typ -> 'a ptr typ
(** Construct a pointer type from an existing type (called the {i reference
    type}).  *)

val ptr_opt : 'a typ -> 'a ptr option typ
(** Construct a pointer type from an existing type (called the {i reference
    type}).  This behaves like {!ptr}, except that null pointers appear in OCaml
    as [None]. *)

val string : string typ
(** A high-level representation of the string type.

    On the C side this behaves like [char *]; on the OCaml side values read
    and written using {!string} are simply native OCaml strings.

    To avoid problems with the garbage collector, values passed using
    {!string} are copied into immovable C-managed storage before being passed
    to C.
*)

val string_opt : string option typ
(** A high-level representation of the string type.  This behaves like {!string},
	except that null pointers appear in OCaml as [None].
*)

(** {4 OCaml pointers} *)

type 'a ocaml = 'a Static.ocaml
(** The type of pointer values pointing directly into OCaml values.
    {b Pointers of this type should never be captured by external code}.
    In particular, functions accepting ['a ocaml] pointers must not invoke
    any OCaml code. *)

val ocaml_string : string ocaml typ
(** Value representing the directly mapped storage of an OCaml string. *)

val ocaml_bytes : Bytes.t ocaml typ
(** Value representing the directly mapped storage of an OCaml byte array. *)

(** {3 Array types} *)

(** {4 C array types} *)

type 'a carray
(** The type of C array values.  A value of type [t carray] can be used to read
    and write array objects in C-managed storage. *)

val array : int -> 'a typ -> 'a carray typ
(** Construct a sized array type from a length and an existing type (called
    the {i element type}). *)

(** {4 Bigarray types} *)

type _ bigarray_class
(** The type of Bigarray classes.  There are four instances, one for each of
    the Bigarray submodules. *)

val genarray :
  < element: 'a;
    ba_repr: 'b;
    bigarray: ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t;
    carray: 'a carray;
    dims: int array > bigarray_class
(** The class of {!Bigarray.Genarray.t} values *)

val array1 :
  < element: 'a;
    ba_repr: 'b;
    bigarray: ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t;
    carray: 'a carray;
    dims: int > bigarray_class
(** The class of {!Bigarray.Array1.t} values *)

val array2 :
  < element: 'a;
    ba_repr: 'b;
    bigarray: ('a, 'b, Bigarray.c_layout) Bigarray.Array2.t;
    carray: 'a carray carray;
    dims: int * int > bigarray_class
(** The class of {!Bigarray.Array2.t} values *)

val array3 :
  < element: 'a;
    ba_repr: 'b;
    bigarray: ('a, 'b, Bigarray.c_layout) Bigarray.Array3.t;
    carray: 'a carray carray carray;
    dims: int * int * int > bigarray_class
(** The class of {!Bigarray.Array3.t} values *)

val bigarray :
  < element: 'a;
    ba_repr: 'b;
    dims: 'dims;
    bigarray: 'bigarray;
    carray: _ > bigarray_class ->
   'dims -> ('a, 'b) Bigarray.kind -> 'bigarray typ
(** Construct a sized bigarray type representation from a bigarray class, the
    dimensions, and the {!Bigarray.kind}. *)

val typ_of_bigarray_kind : ('a, 'b) Bigarray.kind -> 'a typ
(** [typ_of_bigarray_kind k] is the type corresponding to the Bigarray kind
    [k]. *)

(** {3 Function types} *)

type 'a fn = 'a Static.fn
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

(** {3 Struct and union types} *)

type ('a, 'kind) structured = ('a, 'kind) Static.structured
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

val union : string -> 's union typ
(** Construct a new union type.  This behaves analogously to {!structure};
    fields are added with {!(+:+)}. *)

val field : 't typ -> string -> 'a typ ->
  ('a, (('s, [<`Struct | `Union]) structured as 't)) field
(** [field ty label ty'] adds a field of type [ty'] with label [label] to the
    structure or union type [ty] and returns a field value that can be used to
    read and write the field in structure or union instances (e.g. using
    {!getf} and {!setf}).

    Attempting to add a field to a union type that has been sealed with [seal]
    is an error, and will raise {!ModifyingSealedType}. *)

val ( *:* ) : 't typ -> 'a typ -> ('a, (('s, [`Struct]) structured as 't)) field
(** @deprecated Add an anonymous field to a structure.  Use {!field} instead. *)

val ( +:+ ) : 't typ -> 'a typ -> ('a, (('s, [`Union]) structured as 't)) field
(** @deprecated Add an anonymous field to a union.  Use {!field} instead. *)

val seal : (_, [< `Struct | `Union]) structured typ -> unit
(** [seal t] completes the struct or union type [t] so that no further fields
    can be added.  Struct and union types must be sealed before they can be used
    in a way that involves their size or alignment; see the documentation for
    {!IncompleteType} for further details.  *)

(** {3 View types} *)

val view : ?format_typ:((Format.formatter -> unit) -> Format.formatter -> unit) ->
           read:('a -> 'b) -> write:('b -> 'a) -> 'a typ -> 'b typ
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

    The optional argument [format_typ] is used by the {!Ctypes.format_typ} and
    {!string_of_typ} functions to print the type at the top level and
    elsewhere.  If [format_typ] is not supplied the printer for [t] is used
    instead.
*)

(** {3 Abstract types} *)

type 'a abstract
(** The type of abstract values.  The purpose of the [abstract] type is to
    represent values whose type varies from platform to platform.

    For example, the type [pthread_t] is a pointer on some platforms, an
    integer on other platforms, and a struct on a third set of platforms.  One
    way to deal with this kind of situation is to have
    possibly-platform-specific code which interrogates the C type in some way
    to help determine an appropriate representation.  Another way is to use
    [abstract], leaving the representation opaque.

    (Note, however, that although [pthread_t] is a convenient example, since
    the type used to implement it varies significantly across platforms, it's
    not actually a good match for [abstract], since values of type [pthread_t]
    are passed and returned by value.) *)

val abstract : name:string -> size:int -> alignment:int -> 'a abstract typ
(** Create an abstract type specification from the size and alignment
    requirements for the type. *)

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

val ptr_of_raw_address : int64 -> unit ptr
(** Convert the numeric representation of an address to a pointer *)

val raw_address_of_ptr : unit ptr -> int64
(** Return the numeric representation of an address *)

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

