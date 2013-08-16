(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Static

(* Register the closure lookup function with C. *)
let () = Ffi_stubs.set_closure_callback Closure_properties.retrieve

type _ ccallspec =
    Call : bool * (Ctypes_raw.raw_pointer -> 'a) -> 'a ccallspec
  | WriteArg : ('a -> Ctypes_raw.raw_pointer -> unit) * 'b ccallspec ->
               ('a -> 'b) ccallspec

type arg_type = ArgType : 'a Ffi_stubs.ffitype -> arg_type

(* keep_alive ties the lifetimes of objects together.  

   [keep_alive w ~while_live:v] ensures that [w] is not collected while [v] is
   still live.
*)
let keep_alive w ~while_live:v = Gc.finalise (fun _ -> w; ()) v

let rec arg_type : type a. a typ -> arg_type = function
  | Void                             -> ArgType (Ffi_stubs.void_ffitype ()) 
  | Primitive p                      -> ArgType (Ffi_stubs.primitive_ffitype p)
  | Struct ({ spec = Complete _ } as s)
    -> struct_arg_type s
  | Pointer _                        -> ArgType (Ffi_stubs.pointer_ffitype ()) 
  | View { ty }                      -> arg_type ty
  (* The following cases should never happen; aggregate types other than
     complete struct types are excluded during type construction. *)
  | Union _                          -> assert false
  | Array _                          -> assert false
  | Abstract _                       -> assert false
  | Struct { spec = Incomplete _ }   -> assert false
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
                         (Ctypes_raw.raw_pointer -> unit) list ->
                         Ffi_stubs.callspec ->
                         Ctypes_raw.raw_pointer ->
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

let prep_callspec callspec ty =
  let ArgType ctype = arg_type ty in
  Ffi_stubs.prep_callspec callspec ctype

let rec box_function : type a. a fn -> Ffi_stubs.callspec -> a WeakRef.t -> Ffi_stubs.boxedfn
  = fun fn callspec -> match fn with
    | Returns (_, ty) ->
      let () = prep_callspec callspec ty in
      let write_rv = Dynamic.write ty in
      fun f -> Ffi_stubs.Done (write_rv ~offset:0 (WeakRef.get f), callspec)
    | Function (p, f) ->
      let _ = add_argument callspec p in
      let box = box_function f callspec in
      let read = Dynamic.build p ~offset:0 in
      fun f -> Ffi_stubs.Fn (fun buf ->
        try box (WeakRef.make (WeakRef.get f (read buf)))
        with WeakRef.EmptyWeakReference ->
          raise Ffi_stubs.CallToExpiredClosure)

(*
  callspec = allocate_callspec ()
  add_argument callspec arg1
  add_argument callspec arg2
  ...
  add_argument callspec argn
  prep_callspec callspec rettype
*)
let rec build_ccallspec : type a. a fn -> Ffi_stubs.callspec -> a ccallspec
  = fun fn callspec -> match fn with
    | Returns (check_errno, t) ->
      let () = prep_callspec callspec t in
      Call (check_errno, Dynamic.build t ~offset:0)
    | Function (p, f) ->
      let offset = add_argument callspec p in
      let rest = build_ccallspec f callspec in
      WriteArg (Dynamic.write p ~offset, rest)

let build_function ?name fn =
  let c = Ffi_stubs.allocate_callspec () in
  let e = build_ccallspec fn c in
  invoke name e [] c

let ptr_of_rawptr raw_ptr =
  { raw_ptr ; pbyte_offset = 0; reftype = void; pmanaged = None }

let function_of_pointer ?name fn {raw_ptr} =
  build_function ?name fn raw_ptr

let pointer_of_function fn f =
  let cs' = Ffi_stubs.allocate_callspec () in
  let cs = box_function fn cs' in
  let boxed = cs (WeakRef.make f) in
  let id = Closure_properties.record (Obj.repr f) (Obj.repr boxed) in
  ptr_of_rawptr (Ffi_stubs.make_function_pointer cs' id)
