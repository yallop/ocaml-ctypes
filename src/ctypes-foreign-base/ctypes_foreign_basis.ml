(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

module Make(Closure_properties : Ctypes_ffi.CLOSURE_PROPERTIES) =
struct
  open Dl
  open Ctypes

  module Ffi = Ctypes_ffi.Make(Closure_properties)

  exception CallToExpiredClosure = Ctypes_ffi_stubs.CallToExpiredClosure

  let funptr ?(abi=Libffi_abi.default_abi) ?name ?(check_errno=false)
      ?(runtime_lock=false) ?(thread_registration=false) fn =
    let open Ffi in
    let read = function_of_pointer
      ~abi ~check_errno ~release_runtime_lock:runtime_lock ?name fn
    and write = pointer_of_function fn
      ~abi ~acquire_runtime_lock:runtime_lock ~thread_registration in
    Ctypes_static.(view ~read ~write (static_funptr fn))

  let funptr_opt ?abi ?name ?check_errno ?runtime_lock ?thread_registration fn =
    Ctypes_std_views.nullable_funptr_view
      (funptr ?abi ?name ?check_errno ?runtime_lock ?thread_registration fn) fn

  let funptr_of_raw_ptr p = 
    Ctypes.funptr_of_raw_address (Ctypes_ptr.Raw.to_nativeint p)

  let ptr_of_raw_ptr p = 
    Ctypes.ptr_of_raw_address (Ctypes_ptr.Raw.to_nativeint p)

  let foreign_value ?from symbol t =
    from_voidp t (ptr_of_raw_ptr
                    (Ctypes_ptr.Raw.of_nativeint (dlsym ?handle:from ~symbol)))

  let foreign ?(abi=Libffi_abi.default_abi) ?from ?(stub=false)
      ?(check_errno=false) ?(release_runtime_lock=false) symbol typ =
    try
      let coerce = Ctypes_coerce.coerce (static_funptr (void @-> returning void))
        (funptr ~abi ~name:symbol ~check_errno ~runtime_lock:release_runtime_lock typ) in
      coerce (funptr_of_raw_ptr
                (Ctypes_ptr.Raw.of_nativeint
                   (dlsym ?handle:from ~symbol)))
    with
    | exn -> if stub then fun _ -> raise exn else raise exn

  module type Funptr = sig
    type fn
    type t
    val t : t Ctypes.typ
    val t_opt : t option Ctypes.typ
    val free : t -> unit
    val of_fun : ?debug_info:string -> fn -> t
    val with_fun : ?debug_info:string -> fn -> (t -> 'c) -> 'c
  end

  module type Funptr_spec = sig
    type fn_first_arg
    type fn_rest
    val fn : (fn_first_arg -> fn_rest) Ctypes.fn
    val abi : Libffi_abi.abi
    val acquire_runtime_lock : bool
    val thread_registration : bool
  end

  module Make_funptr(Fn : Funptr_spec) :
    Funptr with type fn=(Fn.fn_first_arg -> Fn.fn_rest) = struct
    open Fn
    type fn = fn_first_arg -> fn_rest
    type t = fn Ffi.funptr

    let t =
      let write = Ffi.funptr_to_static_funptr in
      let read = Ffi.funptr_of_static_funptr in
      Ctypes_static.(view ~read ~write (static_funptr fn))

    let t_opt = Ctypes_std_views.nullable_funptr_view t Fn.fn
    let free = Ffi.free_funptr
    let of_fun = Ffi.funptr_of_fun ~abi ~acquire_runtime_lock ~thread_registration fn

    let with_fun ?debug_info f do_it =
      let f = of_fun ?debug_info f in
      match do_it f with
      | res -> free f; res
      | exception exn -> free f; raise exn
  end

  let funptr_spec (type a) (type b) ?(abi=Libffi_abi.default_abi) ?(runtime_lock=false) ?(thread_registration=false) fn : (module Funptr_spec with type fn_first_arg = a and type fn_rest = b) =
    (module struct
         type fn_first_arg = a
         type fn_rest = b
         let fn = fn
         let abi = abi
         let acquire_runtime_lock = runtime_lock
         let thread_registration = thread_registration
       end)

  let call_static_funptr ?name ?(abi=Libffi_abi.default_abi) ?(check_errno=false) ?(release_runtime_lock=false) fn fp =
    Ffi.function_of_pointer
      ?name ~abi ~check_errno ~release_runtime_lock fn fp

end
