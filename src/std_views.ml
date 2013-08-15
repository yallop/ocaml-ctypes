(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

module VStubs = Std_view_stubs

exception CallToExpiredClosure = Ctypes_raw.CallToExpiredClosure

module Function_pointers :
sig
  val view : ?name:string -> ('a -> 'b) Static.fn -> ('a -> 'b) Static.typ
end = struct
  open Static

  (* Register the closure lookup function with C. *)
  let () = VStubs.set_closure_callback Closure_properties.retrieve

  type _ ccallspec =
      Call : bool * (Ctypes_raw.raw_pointer -> 'a) -> 'a ccallspec
    | WriteArg : ('a -> Ctypes_raw.raw_pointer -> unit) * 'b ccallspec ->
                 ('a -> 'b) ccallspec

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
                           VStubs.callspec ->
                           Ctypes_raw.raw_pointer ->
                        a
    = fun name -> function
      | Call (check_errno, read_return_value) ->
        let call = match check_errno, name with
          | true, Some name -> VStubs.call_errno name
          | true, None      -> VStubs.call_errno ""
          | false, _        -> VStubs.call
        in
        fun writers callspec addr ->
          call addr callspec
            (fun buf -> List.iter (fun w -> w buf) writers)
            read_return_value
      | WriteArg (write, ccallspec) ->
        let next = invoke name ccallspec in
        fun writers callspec addr v ->
          next (write v :: writers) callspec addr

  let add_argument : type a. VStubs.callspec -> a typ -> int
    = fun callspec -> function
      | Void -> 0
      | ty   -> let ArgType ctype = arg_type ty in
                VStubs.add_argument callspec ctype

  let prep_callspec callspec ty =
    let ArgType ctype = arg_type ty in
    VStubs.prep_callspec callspec ctype

  let rec box_function : type a. a fn -> VStubs.callspec -> a WeakRef.t -> VStubs.boxedfn
    = fun fn callspec -> match fn with
      | Returns (_, ty) ->
        let () = prep_callspec callspec ty in
        let write_rv = Dynamic.write ty in
        fun f -> VStubs.Done (write_rv ~offset:0 (WeakRef.get f), callspec)
      | Function (p, f) ->
        let _ = add_argument callspec p in
        let box = box_function f callspec in
        let read = Dynamic.build p ~offset:0 in
        fun f -> VStubs.Fn (fun buf ->
          try box (WeakRef.make (WeakRef.get f (read buf)))
          with WeakRef.EmptyWeakReference ->
            raise CallToExpiredClosure)

  (*
    callspec = allocate_callspec ()
    add_argument callspec arg1
    add_argument callspec arg2
    ...
    add_argument callspec argn
    prep_callspec callspec rettype
  *)
  let rec build_ccallspec : type a. a fn -> VStubs.callspec -> a ccallspec
    = fun fn callspec -> match fn with
      | Returns (check_errno, t) ->
        let () = prep_callspec callspec t in
        Call (check_errno, Dynamic.build t ~offset:0)
      | Function (p, f) ->
        let offset = add_argument callspec p in
        let rest = build_ccallspec f callspec in
        WriteArg (Dynamic.write p ~offset, rest)

  let build_function ?name fn =
    let c = VStubs.allocate_callspec () in
    let e = build_ccallspec fn c in
    invoke name e [] c

  let ptr_of_rawptr raw_ptr =
    { raw_ptr ; pbyte_offset = 0; reftype = void; pmanaged = None }

  let function_of_pointer ?name fn {raw_ptr} =
    build_function ?name fn raw_ptr

  let pointer_of_function fn f =
    let cs' = VStubs.allocate_callspec () in
    let cs = box_function fn cs' in
    let boxed = cs (WeakRef.make f) in
    let id = Closure_properties.record (Obj.repr f) (Obj.repr boxed) in
    ptr_of_rawptr (VStubs.make_function_pointer cs' id)

  let format_function_pointer fn k fmt =
    Type_printing.format_fn' fn
      (fun fmt -> Format.fprintf fmt "(*%t)" k) fmt

  let view ?name fn =
    let read = function_of_pointer ?name fn
    and write = pointer_of_function fn
    and format_typ = format_function_pointer fn in
    view ~format_typ ~read ~write (ptr void)
end

let funptr = Function_pointers.view


let string_of_char_ptr {Static.raw_ptr; pbyte_offset} =
  Std_view_stubs.string_of_cstring raw_ptr pbyte_offset

let char_ptr_of_string s =
  let buf = Std_view_stubs.cstring_of_string s in
  { Static.reftype = Static.char;
    pmanaged = Some buf;
    raw_ptr = Dynamic_stubs.block_address buf;
    pbyte_offset = 0 }

let string = Static.(view (ptr char))
  ~read:string_of_char_ptr ~write:char_ptr_of_string

let castp typ p = Dynamic.(from_voidp typ (to_voidp p))

let read_nullable t p = Dynamic.(
  if p = null then None
  else Some !@(castp t (allocate Static.(ptr void) p)))

let write_nullable t = Dynamic.(function
  | None -> null
  | Some f -> !@(castp Static.(ptr void) (allocate t f)))

let nullable_view t =
  let read = read_nullable t
  and write = write_nullable t in
  Static.(view ~read ~write (ptr void))

let funptr_opt fn = nullable_view (funptr fn)

let ptr_opt t = nullable_view (Static.ptr t)

let string_opt = nullable_view string
