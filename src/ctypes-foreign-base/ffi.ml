(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

module type CLOSURE_PROPERTIES =
sig
  val record : Obj.t -> Obj.t -> int
  (** [record c v] links the lifetimes of [c] and [v], ensuring that [v] is not
      collected while [c] is still live.  The return value is a key
      that can be used to retrieve [v] while [v] is still live. *)

  val retrieve : int -> Obj.t
  (** [retrieve v] retrieves a value using a key returned by [record], or raises
      [Not_found] if [v] is no longer live. *)
end

module Make(Closure_properties : CLOSURE_PROPERTIES) =
struct

  open Static
  open Libffi_abi

  (* Register the closure lookup function with C. *)
  let () = Ffi_stubs.set_closure_callback Closure_properties.retrieve

  type _ ccallspec =
      Call : bool * (Ctypes_raw.voidp -> 'a) -> 'a ccallspec
    | WriteArg : ('a -> Ctypes_raw.voidp -> unit) * 'b ccallspec ->
                 ('a -> 'b) ccallspec

  type arg_type = ArgType : 'a Ffi_stubs.ffitype -> arg_type

  (* keep_alive ties the lifetimes of objects together.  

     [keep_alive w ~while_live:v] ensures that [w] is not collected while [v] is
     still live.
  *)
  let keep_alive w ~while_live:v = Gc.finalise (fun _ -> w; ()) v

  let report_unpassable what =
    let msg = Printf.sprintf "libffi does not support passing %s" what in
    raise (Unsupported msg)

  let rec arg_type : type a. a typ -> arg_type = function
    | Void                                -> ArgType (Ffi_stubs.void_ffitype ())
    | Primitive p as prim                 -> let ffitype = Ffi_stubs.primitive_ffitype p in
                                             if ffitype = Ctypes_raw.null
                                             then report_unpassable
                                               (Type_printing.string_of_typ prim)
                                             else ArgType ffitype
    | Pointer _                           -> ArgType (Ffi_stubs.pointer_ffitype ())
    | Union _                             -> report_unpassable "unions"
    | Struct ({ spec = Complete _ } as s) -> struct_arg_type s
    | View { ty }                         -> arg_type ty
    | Array _                             -> report_unpassable "arrays"
    | Bigarray _                          -> report_unpassable "bigarrays"
    | Abstract _                          -> (report_unpassable
                                                "values of abstract type")
    (* The following case should never happen; incomplete types are excluded
       during type construction. *)
    | Struct { spec = Incomplete _ }      -> report_unpassable "incomplete types"
  and struct_arg_type : type s. s structure_type -> arg_type =
     fun ({fields} as s) ->
       let bufspec = Ffi_stubs.allocate_struct_ffitype (List.length fields) in
       (* Ensure that `bufspec' stays alive as long as the type does. *)
       keep_alive bufspec ~while_live:s;
       List.iteri
         (fun i (BoxedField {ftype; foffset}) ->
           let ArgType t = arg_type ftype in
           Ffi_stubs.struct_type_set_argument bufspec i t)
         fields;
       Ffi_stubs.complete_struct_type bufspec;
       ArgType (Ffi_stubs.ffi_type_of_struct_type bufspec)

  (*
    call addr callspec
     (fun buffer ->
          write arg_1 buffer v_1
          write arg buffer v
          ...
          write arg_n buffer v_n)
     read_return_value
  *)
  let rec invoke : type a. string option ->
                           a ccallspec ->
                           (Ctypes_raw.voidp -> unit) list ->
                           Ffi_stubs.callspec ->
                           Ctypes_raw.voidp ->
                        a
    = fun name -> function
      | Call (check_errno, read_return_value) ->
        let call = match check_errno, name with
          | true, Some name -> Ffi_stubs.call_errno name
          | true, None      -> Ffi_stubs.call_errno ""
          | false, _        -> Ffi_stubs.call
        in
        fun writers callspec addr ->
          call addr callspec
            (fun buf -> List.iter (fun w -> w buf) writers)
            read_return_value
      | WriteArg (write, ccallspec) ->
        let next = invoke name ccallspec in
        fun writers callspec addr v ->
          next (write v :: writers) callspec addr

  let add_argument : type a. Ffi_stubs.callspec -> a typ -> int
    = fun callspec -> function
      | Void -> 0
      | ty   -> let ArgType ffitype = arg_type ty in
                Ffi_stubs.add_argument callspec ffitype

  let prep_callspec callspec abi ty =
    let ArgType ctype = arg_type ty in
    Ffi_stubs.prep_callspec callspec (abi_code abi) ctype

  let rec box_function : type a. abi -> a fn -> Ffi_stubs.callspec -> a WeakRef.t ->
      Ffi_stubs.boxedfn
    = fun abi fn callspec -> match fn with
      | Returns ty ->
        let () = prep_callspec callspec abi ty in
        let write_rv = Memory.write ty in
        fun f -> Ffi_stubs.Done (write_rv ~offset:0 (WeakRef.get f), callspec)
      | Function (p, f) ->
        let _ = add_argument callspec p in
        let box = box_function abi f callspec in
        let read = Memory.build p ~offset:0 in
        fun f -> Ffi_stubs.Fn (fun buf ->
          let f' = 
            try WeakRef.get f (read buf) 
            with WeakRef.EmptyWeakReference ->
              raise Ffi_stubs.CallToExpiredClosure
          in
          let v = box (WeakRef.make f') in
          let () = Gc.finalise (fun _ -> f'; ()) v in
          v)

  (*
    callspec = allocate_callspec ()
    add_argument callspec arg1
    add_argument callspec arg2
    ...
    add_argument callspec argn
    prep_callspec callspec rettype
  *)
  let rec build_ccallspec : type a. abi:abi -> check_errno:bool -> a fn ->
    Ffi_stubs.callspec -> a ccallspec
    = fun ~abi ~check_errno fn callspec -> match fn with
      | Returns t ->
        let () = prep_callspec callspec abi t in
        Call (check_errno, Memory.build t ~offset:0)
      | Function (p, f) ->
        let offset = add_argument callspec p in
        let rest = build_ccallspec ~abi ~check_errno f callspec in
        WriteArg (Memory.write p ~offset, rest)

  let build_function ?name ~abi ~check_errno fn =
    let c = Ffi_stubs.allocate_callspec () in
    let e = build_ccallspec ~abi ~check_errno fn c in
    invoke name e [] c

  let ptr_of_rawptr raw_ptr =
    { raw_ptr ; pbyte_offset = 0; reftype = void; pmanaged = None }

  let function_of_pointer ?name ~abi ~check_errno fn =
    let f = build_function ?name ~abi ~check_errno fn in
    fun {raw_ptr} -> f raw_ptr

  let pointer_of_function ~abi fn =
    let cs' = Ffi_stubs.allocate_callspec () in
    let cs = box_function abi fn cs' in
    fun f ->
      let boxed = cs (WeakRef.make f) in
      let id = Closure_properties.record (Obj.repr f) (Obj.repr boxed) in
      ptr_of_rawptr (Ffi_stubs.make_function_pointer cs' id)
end
