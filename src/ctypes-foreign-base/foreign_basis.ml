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

  let funptr ?name ?(check_errno=false) fn =
    let open Ffi in
    let read = function_of_pointer ~check_errno ?name fn
    and write = pointer_of_function fn
    and format_typ = format_function_pointer fn in
    Static.(view ~format_typ ~read ~write (ptr void))

  let castp typ p = Memory.(from_voidp typ (to_voidp p))

  let read_nullable t p = Memory.(
    if p = null then None
    else Some !@(castp t (allocate Static.(ptr void) p)))

  let write_nullable t = Memory.(function
    | None -> null
    | Some f -> !@(castp Static.(ptr void) (allocate t f)))

  let nullable_view t =
    let read = read_nullable t
    and write = write_nullable t in
    Static.(view ~read ~write (ptr void))

  let funptr_opt fn = nullable_view (funptr fn)

  let ptr_of_raw_ptr p = 
    Ctypes.ptr_of_raw_address (Ctypes_raw.PtrType.to_int64 p)

  let foreign_value ?from symbol t =
    from_voidp t (ptr_of_raw_ptr (dlsym ?handle:from ~symbol))

  let foreign ?from ?(stub=false) ?(check_errno=false) symbol typ =
    try
      let p = ptr_of_raw_ptr (dlsym ?handle:from ~symbol) in
      let pp = to_voidp (allocate (ptr void) p) in
      !@ (from_voidp (funptr ~name:symbol ~check_errno typ) pp)
    with 
    | exn -> if stub then fun _ -> raise exn else raise exn
end
