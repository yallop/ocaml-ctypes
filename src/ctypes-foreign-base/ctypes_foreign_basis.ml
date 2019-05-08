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
      Ffi.function_of_pointer ~name:symbol ~abi ~check_errno ~release_runtime_lock typ
        (Ctypes.coerce
           (static_funptr (void @-> returning void))
           (static_funptr typ)
           (funptr_of_raw_ptr
              (Ctypes_ptr.Raw.of_nativeint
                 (dlsym ?handle:from ~symbol))))
    with
    | exn -> if stub then fun _ -> raise exn else raise exn

  type 'a dynamic_funptr = 'a Ffi.funptr

  let dynamic_funptr fn =
    let write = Ffi.funptr_to_static_funptr in
    let read = Ffi.funptr_of_static_funptr in
    Ctypes_static.(view ~read ~write (static_funptr fn))

  let dynamic_funptr_opt fn = Ctypes_std_views.nullable_funptr_view (dynamic_funptr fn) fn


  let free_dynamic_funptr = Ffi.free_funptr

  let dynamic_funptr_of_fun ?debug_info ?(abi=Libffi_abi.default_abi) ?(runtime_lock=false) ?(thread_registration=false) fn f =
    Ffi.funptr_of_fun ?debug_info ~abi ~acquire_runtime_lock:runtime_lock ~thread_registration fn f

  let with_dynamic_funptr_of_fun ?debug_info ?abi ?runtime_lock ?thread_registration fn f do_it =
    let f = dynamic_funptr_of_fun ?debug_info ?abi ?runtime_lock ?thread_registration fn f in
    match do_it f with
    | res -> free_dynamic_funptr f; res
    | exception exn -> free_dynamic_funptr f; raise exn

  let call_static_funptr ?name ?(abi=Libffi_abi.default_abi) ?(check_errno=false) ?(release_runtime_lock=false) fn fp =
    Ffi.function_of_pointer
      ?name ~abi ~check_errno ~release_runtime_lock fn fp

end
