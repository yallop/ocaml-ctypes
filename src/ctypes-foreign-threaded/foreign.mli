(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(** High-level bindings for C functions and values *)

val foreign :
  ?abi:Libffi_abi.abi ->
  ?from:Dl.library ->
  ?stub:bool -> 
  ?check_errno:bool ->
  ?release_runtime_lock:bool ->
  string ->
  ('a -> 'b) Ctypes.fn ->
  ('a -> 'b)
(** [foreign name typ] exposes the C function of type [typ] named by [name] as
    an OCaml value.

    The argument [?from], if supplied, is a library handle returned by
    {!Dl.dlopen}.

    The argument [?stub], if [true] (defaults to [false]), indicates that the
    function should not raise an exception if [name] is not found but return
    an OCaml value that raises an exception when called.

    The value [?check_errno], which defaults to [false], indicates whether
    {!Unix.Unix_error} should be raised if the C function modifies [errno].
    Please note that a function that succeeds is allowed to change errno. So
    use this option with caution.

    The value [?release_runtime_lock], which defaults to [false], indicates
    whether the OCaml runtime lock should be released during the call to the C
    function, allowing other threads to run.  If the runtime lock is released
    then the C function must not access OCaml heap objects, such as arguments
    passed using {!Ctypes.ocaml_string} and {!Ctypes.ocaml_bytes}, and must not
    call back into OCaml.

    @raise Dl.DL_error if [name] is not found in [?from] and [?stub] is
    [false]. *)

val foreign_value : ?from:Dl.library -> string -> 'a Ctypes.typ -> 'a Ctypes.ptr
(** [foreign_value name typ] exposes the C value of type [typ] named by [name]
    as an OCaml value.  The argument [?from], if supplied, is a library handle
    returned by {!Dl.dlopen}.  *)

val funptr :
  ?abi:Libffi_abi.abi ->
  ?name:string ->
  ?check_errno:bool ->
  ?runtime_lock:bool ->
  ?thread_registration:bool ->
  ('a -> 'b) Ctypes.fn ->
  ('a -> 'b) Ctypes.typ
(** Construct a function pointer type from a function type.

    ----

    This function ties the lifetime of the C funtion to the associated OCaml closure.

    An alternative with explicity lifetime management is {!dynamic_funptr}

    ----

    The ctypes library, like C itself, distinguishes functions and function
    pointers.  Functions are not first class: it is not possible to use them
    as arguments or return values of calls, or store them in addressable
    memory.  Function pointers are first class, and so have none of these
    restrictions.

    The value [?check_errno], which defaults to [false], indicates whether
    {!Unix.Unix_error} should be raised if the C function modifies [errno].

    The value [?runtime_lock], which defaults to [false], indicates whether
    the OCaml runtime lock should be released during the call to the C
    function, allowing other threads to run.  If the runtime lock is released
    then the C function must not access OCaml heap objects, such as arguments
    passed using {!Ctypes.ocaml_string} and {!Ctypes.ocaml_bytes}, and must
    not call back into OCaml.  If the function pointer is used to call into
    OCaml from C then the [?runtime_lock] argument indicates whether the lock
    should be acquired and held during the call.

    @raise Dl.DL_error if [name] is not found in [?from] and [?stub] is
    [false]. *)

val funptr_opt :
  ?abi:Libffi_abi.abi ->
  ?name:string ->
  ?check_errno:bool ->
  ?runtime_lock:bool ->
  ?thread_registration:bool ->
  ('a -> 'b) Ctypes.fn ->
  ('a -> 'b) option Ctypes.typ
(** Construct a function pointer type from a function type.

    This behaves like {!funptr}, except that null pointers appear in OCaml as
    [None]. *)

exception CallToExpiredClosure
(** A closure passed to C was collected by the OCaml garbage collector before
    it was called. *)

module type Funptr = sig
  type fn
  (** [fn] is the signature of the underlying OCaml function. *)

  type t
  (** Handle to an OCaml function that can be passed to C for use in callbacks.

      Each value of type {!t} allocated by {!of_fun} must be deallocated by calling {!free}.
      Alternatively {!with_fun} encapsulates both allocation and deallocation. *)

  val t : t Ctypes.typ
  (** A type representation for a function pointer type with explicit lifetime management. *)

  val t_opt : t option Ctypes.typ
  (** This behaves like {!t}, except that null pointers appear in OCaml as [None]. *)

  val free : t -> unit
  (** Indicate that the [fptr] is no longer needed.

      Once [free] has been called any C calls to this [Funptr.t] are
      unsafe. Only call [free] once the callback is no longer used from C. *)

  val of_fun : fn -> t
  (** Turn an OCaml closure into a function pointer that can be passed to C.

      You MUST call {!free} when the function pointer is no longer needed.
      Failure to do so will result in a memory leak.

      Failure to call {!free} and not holding a reference this this pointer
      is an error.

      Alternatively {!with_fun} encapsulates both allocation and deallocation.

      Implementation detail: To avoid hard to debug crashes the implementation
      will leak the OCaml closure in this event that {!free} was not used and
      report a warning, see {!on_leaked_funptr}. *)

  val with_fun : fn -> (t -> 'c) -> 'c
  (** [with_fun fn (fun fptr -> DO_STUFF)] - Turn an OCaml closure into a
      function pointer and do simple life cycle management.

      This will automatically call [free fptr] after [DO_STUFF] completes.

      [with_fun] is not safe to use if the C function ptr [fptr] may still be used
      after [DO_STUFF] completes.
  *)
end

val dynamic_funptr
  :  ?abi:Libffi_abi.abi
  -> ?runtime_lock:bool
  -> ?thread_registration:bool
  -> ('a -> 'b) Ctypes.fn
  -> (module Funptr with type fn = 'a->'b)
(** Define a type representation for more safely passing OCaml functions to C.

    [(val (dynamic_funptr (FOO @-> returning BAR)))] is roughly equivalent to
    [BAR( * )(FOO)] in C.

    Example:
    {[
      module Progress_callback = (val (dynamic_funptr (int @-> int @-> ptr void @-> returning void)))
      let keygen =
        foreign "RSA_generate_key" (int @-> int @-> Progress_callback.t @-> ptr void @-> returning rsa_key)
      let secret_key =
        Progress_callback.with_fun
          (fun a b _ -> printf "progress: a:%d, b:%d\n" a b)
          (fun progress ->
             keygen 2048 65537 progress null)
    ]}
*)

val report_leaked_funptr : (string -> unit) ref
(** Hook for setting custom handling for leaked non-{!free}d {!dynamic_funptr}s.

    By default the library will retain function pointers that have not been freed and
    print an warning to stderr.

    You can use this hook to change how these error messages are reported. *)
