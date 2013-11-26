(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

module Make(Closure_properties : Ffi.CLOSURE_PROPERTIES) =
struct
  open Dl
  open Ctypes_base.Ctypes

  module Ffi = Ffi.Make(Closure_properties)

  exception CallToExpiredClosure = Ffi_stubs.CallToExpiredClosure

  let format_function_pointer fn k fmt =
    Ctypes_base.Type_printing.format_fn' fn
      (fun fmt -> Format.fprintf fmt "(*%t)" k) fmt

  let funptr ?name ?(check_errno=false) fn =
    let open Ffi in
    let read = function_of_pointer ~check_errno ?name fn
    and write = pointer_of_function fn
    and format_typ = format_function_pointer fn in
    Ctypes_base.Static.(view ~format_typ ~read ~write (ptr void))

  let castp typ p = Ctypes_base.Memory.(from_voidp typ (to_voidp p))

  let funptr_opt fn = Ctypes_base.Std_views.nullable_view (funptr fn)

  let ptr_of_raw_ptr p = 
    Ctypes_base.Ctypes.ptr_of_raw_address (Ctypes_base.Ctypes_raw.PtrType.to_int64 p)

  let foreign_value ?from symbol t =
    from_voidp t (ptr_of_raw_ptr (dlsym ?handle:from ~symbol))

  let foreign ?from ?(stub=false) ?(check_errno=false) symbol typ =
    try
      let coerce = Ctypes_base.Coerce.coerce (ptr void) (funptr ~name:symbol ~check_errno typ) in
      coerce (ptr_of_raw_ptr (dlsym ?handle:from ~symbol))
    with 
    | exn -> if stub then fun _ -> raise exn else raise exn
end
