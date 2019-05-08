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
    This function hides hard to reason about details of the life-time of the ocaml closure
    and C function pointer. Getting this wrong will cause hard to debug segmentation faults.

    For passing ocaml functions into C (for use in callbacks) consider using [Foreign.dynamic_funptr].

    If passing function pointers from C [Ctypes.static_funptr] and [Foreign.call_static_funptr]
    can be used.
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

type 'a dynamic_funptr
(** [dynamic_funptr] allows safer passing of ocaml functions for use as C callbacks.

    Unfortunately it is not possible to track if a fuction pointer is still used in C,
    so you must use the appropriate life cycle functions below for this.

    See [dynamic_funptr], [dynamic_funptr_of_fun], [free_dynamic_funptr]
    and [with_dynamic_funptr_of_fun].
*)

val dynamic_funptr : ('a -> 'b) Ctypes.fn -> ('a -> 'b) dynamic_funptr Ctypes.typ
(** [dynamic_funptr fn] - define a Ctype for more safely passing ocaml functions to C.

    [dynamic_funptr (FOO @-> returning BAR)] is roughly equivalent to [BAR( * )(FOO)] in C.
*)

val dynamic_funptr_opt : ('a -> 'b) Ctypes.fn -> ('a -> 'b) dynamic_funptr option Ctypes.typ
(** Like [dynamic_funptr] but optional. *)

val free_dynamic_funptr : _ dynamic_funptr -> unit
(** [free_dynamic_funptr fptr] - Indicate that the [fptr] is no longer needed.

    Once [free_dynamic_funptr] has been called any C calls to this [dynamic_funptr] are
    unsafe and may result in a segmentation fault. Only call [free_dynamic_funptr] once
    the callback is no longer used from C.
*)

val dynamic_funptr_of_fun : ?debug_info:string -> ?abi:Libffi_abi.abi -> ?runtime_lock:bool -> ?thread_registration:bool
  -> ('a -> 'b) Ctypes.fn -> ('a -> 'b) -> ('a -> 'b) dynamic_funptr
(** [dynamic_funptr_of_fun fn] - Turn an ocaml closure into a function pointer
    that can be passed to C.

    You MUST call [free_dynamic_funptr] to the function pointer is no longer needed.
    Failure to do so will result in a memory leak.
    Failure to call [free_dynamic_funptr] and not holding a reference this this pointer
    is an error. To avoid unexpected crashes we log this and ensure that potentially still
    needed ocaml values are retained.

    For many use cases it may be simpler to use [with_dynamic_funptr_of_fun] instead as
    this will do the free for you. *)

val with_dynamic_funptr_of_fun : ?debug_info:string -> ?abi:Libffi_abi.abi -> ?runtime_lock:bool -> ?thread_registration:bool
  -> ('a -> 'b) Ctypes.fn -> ('a -> 'b) -> (('a -> 'b) dynamic_funptr -> 'c) -> 'c
(** [with_dynamic_funptr_of_fun fn (fun fptr -> DO_STUFF)] - Turn an ocaml closure into a
    function pointer and do simple life cycle management.

    This will automatically call [free_dynamic_funptr fptr] after [DO_STUFF] completes.

    It is not safe to use if the C function ptr [fptr] may still be used after this
    has been released.
*)

val call_static_funptr :
  ?name:string ->
  ?abi:Libffi_abi.abi ->
  ?check_errno:bool ->
  ?release_runtime_lock:bool ->
  ('a -> 'b) Ctypes.fn ->
  ('a -> 'b) Ctypes.static_funptr -> 'a -> 'b
(** [call_static_funptr fn fptr] - Helper to allow calling function pointers passed to
    ocaml from C. *)
