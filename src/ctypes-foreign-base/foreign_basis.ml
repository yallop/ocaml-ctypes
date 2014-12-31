(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

module Make(Closure_properties : Ffi.CLOSURE_PROPERTIES) =
struct
  open Dl
  open Ctypes

  module Ffi = Ffi.Make(Closure_properties)

  exception CallToExpiredClosure = Ffi_stubs.CallToExpiredClosure

  let format_function_pointer fn k fmt =
    Type_printing.format_fn' fn
      (fun fmt -> Format.fprintf fmt "(*%t)" k) fmt

  let funptr ?(abi=Libffi_abi.default_abi) ?name ?(check_errno=false)
      ?(runtime_lock=false) fn =
    let open Ffi in
    let read = function_of_pointer
      ~abi ~check_errno ~release_runtime_lock:runtime_lock ?name fn
    and write = pointer_of_function
      ~abi ~acquire_runtime_lock:runtime_lock fn
    and format_typ = format_function_pointer fn in
    Static.(view ~format_typ ~read ~write (ptr void))

  let funptr_opt ?abi fn = Std_views.nullable_view (funptr ?abi fn) void

  let ptr_of_raw_ptr p = 
    Ctypes.ptr_of_raw_address (Ctypes_ptr.Raw.to_nativeint p)

  let foreign_value ?from symbol t =
    from_voidp t (ptr_of_raw_ptr (dlsym ?handle:from ~symbol))

  let foreign ?(abi=Libffi_abi.default_abi) ?from ?(stub=false)
      ?(check_errno=false) ?(release_runtime_lock=false) symbol typ =
    try
      let coerce = Coerce.coerce (ptr void)
        (funptr ~abi ~name:symbol ~check_errno ~runtime_lock:release_runtime_lock typ) in
      coerce (ptr_of_raw_ptr (dlsym ?handle:from ~symbol))
    with 
    | exn -> if stub then fun _ -> raise exn else raise exn

  type 'a fn = 'a
  let view_invoke f x = f x
end
