(* Low-level unsafe interface for calling functions using libffi. *)

module Types :
sig
  type 'a ctype
  external sizeof : _ ctype -> int = "ctypes_sizeof"

  type voidp
  val null : voidp

  val pointer : voidp ctype

  val void : unit ctype
  val char : char ctype
  val float : float ctype
  val double : float ctype
  val int : int ctype
  val nativeint : nativeint ctype
  val int8_t : int ctype
  val int16_t : int ctype
  val short : int ctype
  val int32_t : int32 ctype
  val int64_t : int64 ctype
  val uchar : int ctype
  val uint8_t : int ctype

  val string : string ctype
end =
struct
  type 'a ctype
  type voidp

  external sizeof : _ ctype -> int = "ctypes_sizeof"

  external _int_type_info : unit -> int ctype = "ctypes_int_type_info"
  let int = _int_type_info ()

  external _nativeint_type_info : unit -> nativeint ctype = "ctypes_nativeint_type_info"
  let nativeint = _nativeint_type_info ()

  external _double_type_info : unit -> float ctype = "ctypes_double_type_info"
  let double = _double_type_info ()

  external _voidp_type_info : unit -> voidp ctype = "ctypes_voidp_type_info"
  let pointer = _voidp_type_info ()

  external _void_type_info : unit -> unit ctype = "ctypes_void_type_info"
  let void = _void_type_info ()

  external _int8_t_type_info : unit -> int ctype = "ctypes_int8_t_type_info"
  let int8_t = _int8_t_type_info()

  external _int16_t_type_info : unit -> int ctype = "ctypes_int16_t_type_info"
  let int16_t = _int16_t_type_info()

  external _uint8_t_type_info : unit -> int ctype = "ctypes_uint8_t_type_info"
  let uint8_t = _uint8_t_type_info()

  external _char_type_info : unit -> char ctype = "ctypes_char_type_info"
  let char = _char_type_info()

  external _uchar_type_info : unit -> int ctype = "ctypes_uchar_type_info"
  let uchar = _uchar_type_info()

  external _short_type_info : unit -> int ctype = "ctypes_short_type_info"
  let short = _short_type_info()

  external _int32_t_type_info : unit -> int32 ctype = "ctypes_int32_t_type_info"
  let int32_t = _int32_t_type_info()

  external _int64_t_type_info : unit -> int64 ctype = "ctypes_int64_t_type_info"
  let int64_t = _int64_t_type_info()

  external _float_type_info : unit -> float ctype = "ctypes_float_type_info"
  let float = _float_type_info()

  external _string_type_info : unit -> string ctype = "ctypes_string_type_info"
  let string = _string_type_info()

  external _null : unit -> voidp = "ctypes_null_value"
  let null = _null ()
end

open Types

(* A specification of argument C-types and C-return values *)
type bufferspec

(* An immediate pointer to a block of memory, not managed by the
   garbage collector, into which we can read/write C values *)
type immediate_pointer = voidp

(* Read a C value from a block of memory *)
external read : offset:int -> 'a ctype -> immediate_pointer -> 'a
  = "ctypes_read"

(* Write a C value to a block of memory *)
external write : offset:int -> 'a ctype -> immediate_pointer -> 'a -> unit
  = "ctypes_write"

(* Allocate a new C call specification *)
external allocate_callspec : unit -> bufferspec
  = "ctypes_allocate_callspec"

(* Pass the return type and conclude the specification preparation *)
external prep_callspec : bufferspec -> _ ctype -> unit
  = "ctypes_prep_callspec"

(* Call the function specified by `bufferspec' at the given address.
   The callback functions write the arguments to the buffer and read
   the return value. *)
external call : immediate_pointer -> bufferspec -> (immediate_pointer -> unit) -> (immediate_pointer -> 'a) -> 'a
  = "ctypes_call"

type managed_buffer
type _ structure = managed_buffer

(* Allocate a new C typed buffer specification *)
external allocate_bufferspec : unit -> bufferspec
  = "ctypes_allocate_bufferspec"

(* Produce a structure type representation from the buffer specification. *)
external complete_struct_type : bufferspec -> _ structure ctype
  = "ctypes_complete_structspec"

(* Add an argument to the C buffer specification *)
external add_argument : bufferspec -> _ ctype -> int
  = "ctypes_add_argument"

(* nary callbacks *)
type boxedfn =
  | Done of (immediate_pointer -> unit)
  | Fn of (immediate_pointer -> boxedfn)

(* Construct a pointer to a boxed n-ary function *)
external make_function_pointer : bufferspec -> boxedfn -> immediate_pointer
  = "ctypes_make_function_pointer"

(* Allocate a region of stable memory managed by a custom block. *)
external allocate : int -> managed_buffer
  = "ctypes_allocate"

(* Obtain the address of the managed block. *)
external managed_secret : managed_buffer -> immediate_pointer
  = "ctypes_managed_secret"

(* An internal error: for example, an `ffi_type' object passed to
   ffi_prep_cif was incorrect. *)
exception Ffi_internal_error of string
let () = Callback.register_exception "FFI_internal_error"
  (Ffi_internal_error "")

let () = at_exit Gc.major (* TODO: remove this after testing *)
