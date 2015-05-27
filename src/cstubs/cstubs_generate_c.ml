(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* C stub generation *)

open Ctypes_static
open Cstubs_c_language

let max_byte_args = 5

(* We're using an abstract type ([value]) as an argument and return type, so
   we'll use the [Function] and [Return] constructors directly.  The smart
   constructors [@->] and [returning] would reject the abstract type. *)
let (@->) f t = Function (f, t)
let returning t = Returns t

(* C stub customization options. These options are documented in
   [Cstubs_inverted]. *)
type inverted_stubs_options =
  {
    (* will wrap [caml_release_runtime_system] and
       [caml_acquire_runtime_system] around callbacks.*)
    runtime_lock: bool;

    (* will wrap each function call with [caml_c_thread_register] and
       [caml_c_thread_unregister]. *)
    c_thread_register: bool;

    (* Arbitrary C code to be inserted before/after the
       function. Typically, can be used to insert mutex
       acquire/release. *)
    prelude: string option;
    epilogue: string option;

  }

let value = abstract ~name:"value" ~size:0 ~alignment:0

module Generate_C =
struct
  let report_unpassable what =
    let msg = Printf.sprintf "cstubs does not support passing %s" what in
    raise (Unsupported msg)

  let reader fname fn = { fname; allocates = false; reads_ocaml_heap = true; fn = Fn fn }
  let conser fname fn = { fname; allocates = true; reads_ocaml_heap = false; fn = Fn fn }
  let immediater fname fn = { fname; allocates = false; reads_ocaml_heap = false; fn = Fn fn }

  let local name ty = `Local (name, Ty ty)

  let rec (>>=) : type a. ccomp * a typ -> (cexp -> ccomp) -> ccomp =
   fun (e, ty) k ->
     let x = fresh_var () in
     match e with
       (* let x = v in e ~> e[x:=v] *)
     | #cexp as v ->
       k v
     | #ceff as e ->
       `Let ((local x ty, e), k (local x ty))
     | `LetConst (y, i, c) ->
       (* let x = (let const y = i in c) in e
          ~>
          let const y = i in (let x = c in e) *)
       let Ty t = Type_C.ccomp c in
       `LetConst (y, i, (c, t) >>= k)
     | `CAMLreturnT (Ty ty, v) ->
       (k v, ty) >>= fun e ->
       `CAMLreturnT (Type_C.cexp e, e)
     | `Let (ye, c) ->
       (* let x = (let y = e1 in e2) in e3
          ~>
          let y = e1 in (let x = e2 in e3) *)
       let Ty t = Type_C.ccomp c in
       `Let (ye, (c, t) >>= k)

  let (>>) c1 c2 = (c1, Void) >>= fun _ -> c2

  let prim_prj : type a. a Ctypes_primitive_types.prim -> _ =
    let open Ctypes_primitive_types in function
    | Char -> reader "Int_val" (value @-> returning int)
    | Schar -> reader "Int_val" (value @-> returning int)
    | Uchar -> reader "Uint8_val" (value @-> returning uint8_t)
    | Bool -> reader "Bool_val" (value @-> returning bool)
    | Short -> reader "Int_val" (value @-> returning int)
    | Int -> reader "Int_val" (value @-> returning int)
    | Long -> reader "ctypes_long_val" (value @-> returning long)
    | Llong -> reader "ctypes_llong_val" (value @-> returning llong)
    | Ushort -> reader "ctypes_ushort_val" (value @-> returning ushort)
    | Uint -> reader "ctypes_uint_val" (value @-> returning uint)
    | Ulong -> reader "ctypes_ulong_val" (value @-> returning ulong)
    | Ullong -> reader "ctypes_ullong_val" (value @-> returning ullong)
    | Size_t -> reader "ctypes_size_t_val" (value @-> returning size_t)
    | Int8_t -> reader "Int_val" (value @-> returning int)
    | Int16_t -> reader "Int_val" (value @-> returning int)
    | Int32_t -> reader "Int32_val" (value @-> returning int32_t)
    | Int64_t -> reader "Int64_val" (value @-> returning int64_t)
    | Uint8_t -> reader "Uint8_val" (value @-> returning uint8_t)
    | Uint16_t -> reader "Uint16_val" (value @-> returning uint16_t)
    | Uint32_t -> reader "Uint32_val" (value @-> returning uint32_t)
    | Uint64_t -> reader "Uint64_val" (value @-> returning uint64_t)
    | Camlint -> reader "Int_val" (value @-> returning int)
    | Nativeint -> reader "Nativeint_val" (value @-> returning nativeint)
    | Float -> reader "Double_val" (value @-> returning double)
    | Double -> reader "Double_val" (value @-> returning double)
    | Complex32 -> reader "ctypes_float_complex_val" (value @-> returning complex32)
    | Complex64 -> reader "ctypes_double_complex_val" (value @-> returning complex64)

  let prim_inj : type a. a Ctypes_primitive_types.prim -> _ =
    let open Ctypes_primitive_types in function
    | Char -> immediater "Val_int" (int @-> returning value)
    | Schar -> immediater "Val_int" (int @-> returning value)
    | Uchar -> conser "ctypes_copy_uint8" (uint8_t @-> returning value)
    | Bool -> immediater "Val_bool" (bool @-> returning value)
    | Short -> immediater "Val_int" (int @-> returning value)
    | Int -> immediater "Val_int" (int @-> returning value)
    | Long -> conser "ctypes_copy_long" (long @-> returning value)
    | Llong -> conser "ctypes_copy_llong" (llong @-> returning value)
    | Ushort -> conser "ctypes_copy_ushort" (ushort @-> returning value)
    | Uint -> conser "ctypes_copy_uint" (uint @-> returning value)
    | Ulong -> conser "ctypes_copy_ulong" (ulong @-> returning value)
    | Ullong -> conser "ctypes_copy_ullong" (ullong @-> returning value)
    | Size_t -> conser "ctypes_copy_size_t" (size_t @-> returning value)
    | Int8_t -> immediater "Val_int" (int @-> returning value)
    | Int16_t -> immediater "Val_int" (int @-> returning value)
    | Int32_t -> conser "caml_copy_int32" (int32_t @-> returning value)
    | Int64_t -> conser "caml_copy_int64" (int64_t @-> returning value)
    | Uint8_t -> conser "ctypes_copy_uint8" (uint8_t @-> returning value)
    | Uint16_t -> conser "ctypes_copy_uint16" (uint16_t @-> returning value)
    | Uint32_t -> conser "ctypes_copy_uint32" (uint32_t @-> returning value)
    | Uint64_t -> conser "ctypes_copy_uint64" (uint64_t @-> returning value)
    | Camlint -> immediater "Val_int" (int @-> returning value)
    | Nativeint -> conser "caml_copy_nativeint" (nativeint @-> returning value)
    | Float -> conser "caml_copy_double" (double @-> returning value)
    | Double -> conser "caml_copy_double" (double @-> returning value)
    | Complex32 -> conser "ctypes_copy_float_complex" (complex32 @-> returning value)
    | Complex64 -> conser "ctypes_copy_double_complex" (complex64 @-> returning value)

  let of_fatptr : cexp -> ccomp =
    fun x -> `App (reader "CTYPES_ADDR_OF_FATPTR"
                          (value @-> returning (ptr void)),
                   [x])

  let string_to_ptr : cexp -> ccomp =
    fun x -> `App (reader "CTYPES_PTR_OF_OCAML_STRING"
                          (value @-> returning (ptr void)),
                   [x])

  let float_array_to_ptr : cexp -> ccomp =
    fun x -> `App (reader "CTYPES_PTR_OF_FLOAT_ARRAY"
                          (value @-> returning (ptr void)),
                   [x])

  let from_ptr : cexp -> ceff =
    fun x -> `App (conser "CTYPES_FROM_PTR"
                          (ptr void @-> returning value),
                   [x])

  let acquire_runtime_system : ccomp =
    `App (conser "caml_acquire_runtime_system" (ptr void @-> returning void), [])

  let release_runtime_system : ccomp =
    `App (conser "caml_release_runtime_system" (ptr void @-> returning void), [])

  let c_thread_register : ccomp =
    `App (conser "caml_c_thread_register" (ptr void @-> returning void), [])

  let c_thread_unregister : ccomp =
    `App (conser "caml_c_thread_unregister" (ptr void @-> returning void), [])

  let val_unit : ceff = `Global { name = "Val_unit";
                                  references_ocaml_heap = true;
                                  typ = Ty value }

  let functions : ceff = `Global
    { name = "functions";
      references_ocaml_heap = true;
      typ = Ty (ptr value) }

  let caml_callbackN : cfunction =
    { fname = "caml_callbackN";
      allocates = true;
      reads_ocaml_heap = true;
      fn = Fn (value @-> int @-> ptr value @-> returning value) }

  let copy_bytes : cfunction =
    { fname = "ctypes_copy_bytes";
      allocates = true;
      reads_ocaml_heap = true;
      fn = Fn (ptr void @-> size_t @-> returning value) }

  let cast : type a b. from:ty -> into:ty -> ccomp -> ccomp =
    fun ~from:(Ty from) ~into e ->
      (e, from) >>= fun x ->
      `Cast (into, x)

  let rec prj : type a b. a typ -> orig: b typ -> cexp -> ccomp option =
    fun ty ~orig x -> match ty with
    | Void -> None
    | Primitive p ->
      let { fn = Fn fn } as prj = prim_prj p in
      let rt = return_type fn in
      Some (cast ~from:rt ~into:(Ty (Primitive p)) (`App (prj, [x])))
    | Pointer _ -> Some (of_fatptr x)
    | Struct s ->
      Some ((of_fatptr x, ptr void) >>= fun y ->
            `Deref (`Cast (Ty (ptr orig), y)))
    | Union u ->
      Some ((of_fatptr x, ptr void) >>= fun y ->
            `Deref (`Cast (Ty (ptr orig), y)))
    | Abstract _ -> report_unpassable "values of abstract type"
    | View { ty } -> prj ty ~orig x
    | Array _ -> report_unpassable "arrays"
    | Bigarray _ -> report_unpassable "bigarrays"
    | OCaml String -> Some (string_to_ptr x)
    | OCaml Bytes -> Some (string_to_ptr x)
    | OCaml FloatArray -> Some (float_array_to_ptr x)

  let prj ty x = prj ty ~orig:ty x

  let rec inj : type a. a typ -> clocal -> ceff =
    fun ty x -> match ty with
    | Void -> val_unit
    | Primitive p -> `App (prim_inj p, [`Cast (Ty (Primitive p), (x :> cexp))])
    | Pointer _ -> from_ptr (x:> cexp)
    | Struct s -> `App (copy_bytes, [`Addr (x :> cvar); `Int (sizeof ty)])
    | Union u -> `App (copy_bytes, [`Addr (x :> cvar); `Int (sizeof ty)])
    | Abstract _ -> report_unpassable "values of abstract type"
    | View { ty } -> inj ty x
    | Array _ -> report_unpassable "arrays"
    | Bigarray _ -> report_unpassable "bigarrays"
    | OCaml _ -> report_unpassable "ocaml references as return values"

  type _ fn =
  | Returns  : 'a typ   -> 'a fn
  | Function : string * 'a typ * 'b fn  -> ('a -> 'b) fn

  let rec name_params : type a. a Ctypes_static.fn -> a fn = function
    | Ctypes_static.Returns t -> Returns t
    | Ctypes_static.Function (f, t) -> Function (fresh_var (), f, name_params t)

  let rec value_params : type a. a fn -> (string * ty) list = function
    | Returns t -> []
    | Function (x, _, t) -> (x, Ty value) :: value_params t

  let fundec : type a. string -> a Ctypes.fn -> cfundec =
    fun name fn -> `Fundec (name, args fn, return_type fn)

  let fn : type a. cname:string -> stub_name:string -> a Ctypes_static.fn -> cfundef =
    fun ~cname ~stub_name f ->
      let fvar = { fname = cname;
                   allocates = false;
                   reads_ocaml_heap = false;
                   fn = Fn f; } in
      let rec body : type a. _ -> a fn -> _ =
         fun vars -> function
         | Returns t ->
           let x = fresh_var () in
           let e, ty = `App (fvar, (List.rev vars :> cexp list)), t in
           let k = fun x -> (inj t x :> ccomp)  in
           `Let ((local x ty, e), k (local x ty))
         | Function (x, f, t) ->
           begin match prj f (local x value) with
             None -> body vars t
           | Some projected ->
             (projected, f) >>= fun x' ->
             body (x' :: vars) t
           end
      in
      let f' = name_params f in
      `Function (`Fundec (stub_name, value_params f', Ty value),
                 body [] f')

  let byte_fn : type a. string -> a Ctypes_static.fn -> int -> cfundef =
    fun fname fn nargs ->
      let argv = ("argv", Ty (ptr value)) in
      let argc = ("argc", Ty int) in
      let f = { fname ;
                allocates = true;
                reads_ocaml_heap = true;
                fn = Fn fn }
      in
      let rec build_call ?(args=[]) = function
        | 0 -> `App (f, args)
        | n -> (`Index (`Local argv, `Int (n - 1)), value) >>= fun x ->
               build_call ~args:(x :: args) (n - 1)
      in
      let bytename = Printf.sprintf "%s_byte%d" fname nargs in
      `Function (`Fundec (bytename, [argv; argc], Ty value),
                 build_call nargs)

  let inverse_fn ~stub_name ~options f =
    let `Fundec (_, args, Ty rtyp) as dec = fundec stub_name f in
    let idx = local (Printf.sprintf "fn_%s" stub_name) int in
    let project typ e =
      match prj typ e with
        None -> (e :> ccomp)
      | Some e -> e
    in
    let wrap_if cond (lft:ccomp) (rgt:ccomp) =
      if cond then lft >> rgt else rgt
    in
    let wrap_inline (opt: string option) c =
      match opt with
      | None -> c
      | Some i -> `Inline i >> c
    in
    let call =
      (* f := functions[fn_name];
         x := caml_callbackN(f, nargs, locals);
         y := T_val(x);
         CAMLdrop();
         y *)
      (`Index (functions, idx), value) >>= fun f ->
      (`App (caml_callbackN, [f;
                              local "nargs" int;
                              local "locals" (ptr value)]),
       value) >>= fun x ->
      (project rtyp x, rtyp) >>= fun y ->
      (`CAMLdrop, void) >>= fun _ ->
      wrap_if options.runtime_lock release_runtime_system (
      wrap_if options.c_thread_register c_thread_unregister (
      wrap_inline options.epilogue
      (y :> ccomp)))
    in
    let body =
      (* locals[0] = Val_T0(x0);
         locals[1] = Val_T1(x1);
         ...
         locals[n] = Val_Tn(xn);
         call;       *)
      snd
        (ListLabels.fold_right  args
           ~init:(List.length args - 1, call)
           ~f:(fun (x, Ty t) (i, c) ->
             i - 1,
             `Assign (`Index (local "locals" (ptr value), `Int i),
                      (inj t (local x t))) >> c))
    in
      (* T f(T0 x0, T1 x1, ..., Tn xn) {
            enum { nargs = n };
            CAMLparam0();
            CAMLlocalN(locals, nargs);
            body
         }      *)
    `Function
      (dec,
       `LetConst (local "nargs" int, `Int (List.length args),
                  wrap_inline options.prelude @@
                  wrap_if options.c_thread_register c_thread_register (
                  wrap_if options.runtime_lock acquire_runtime_system (
                        `CAMLparam0 >>
                        `CAMLlocalN (local "locals" (array (List.length args) value),
                                     local "nargs" int) >>
                        body)
                  )))

  let value : type a. cname:string -> stub_name:string -> a Ctypes_static.typ -> cfundef =
    fun ~cname ~stub_name typ ->
      let (e, ty) = (`Addr (`Global { name = cname; typ = Ty typ;
                                      references_ocaml_heap = false }), (ptr typ)) in
      let x = fresh_var () in
      `Function (`Fundec (stub_name, ["_", Ty value], Ty value),
                 `Let ((local x ty, e),
                       (inj (ptr typ) (local x ty) :> ccomp)))

end

let fn ~cname ~stub_name fmt fn =
  let `Function (`Fundec (f, xs, _), _) as dec
      = Generate_C.fn ~stub_name ~cname fn
  in
  let nargs = List.length xs in
  if nargs > max_byte_args then begin
    Cstubs_emit_c.cfundef fmt dec;
    Cstubs_emit_c.cfundef fmt (Generate_C.byte_fn f fn nargs)
  end
  else
    Cstubs_emit_c.cfundef fmt dec

let value ~cname ~stub_name fmt typ =
  let dec = Generate_C.value ~cname ~stub_name typ in
  Cstubs_emit_c.cfundef fmt dec

let inverse_fn ~stub_name ~options fmt fn : unit =
  Cstubs_emit_c.cfundef fmt (Generate_C.inverse_fn ~stub_name ~options fn)

let inverse_fn_decl ~stub_name fmt fn =
  Format.fprintf fmt "@[%a@];@\n"
    Cstubs_emit_c.cfundec (Generate_C.fundec stub_name fn)
