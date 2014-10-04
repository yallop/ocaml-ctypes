(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* C stub generation *)

open Static
open Cstubs_c_language

let max_byte_args = 5

(* We're using an abstract type ([value]) as an argument and return type, so
   we'll use the [Function] and [Return] constructors directly.  The smart
   constructors [@->] and [returning] would reject the abstract type. *)
let (@->) f t = Function_ (f, t)
let returning t = Returns_ t

module Generate_C =
struct
  let report_unpassable what =
    let msg = Printf.sprintf "cstubs does not support passing %s" what in
    raise (Unsupported msg)

  let reader fname fn = { fname; allocates = false; reads_ocaml_heap = true; fn }
  let conser fname fn = { fname; allocates = true; reads_ocaml_heap = false; fn }
  let immediater fname fn = { fname; allocates = false; reads_ocaml_heap = false; fn }

  let local ?(references_ocaml_heap=true) name typ =
    {name; typ; references_ocaml_heap}

  let val_unit : value ceff = CGlobal { name = "Val_unit";
                                        references_ocaml_heap = true;
                                        typ = value }

  let rec (>>=) : type a b. a ccomp (* * a typ *) -> (a cexp -> b ccomp) -> b ccomp =
   fun e k ->
     let x = fresh_var () in
     match e with
       (* let x = v in e ~> e[x:=v] *) 
     | CEff (CExp v) ->
        k v
     | CEff e ->
        let ty = Type_C.ceff e in
        CLet (CBind (local x ty, e), k (CLocal (local x ty)))
     | CLetConst (y, i, c) ->
       (* let x = (let const y = i in c) in e
          ~>
          let const y = i in (let x = c in e) *)
       CLetConst (y, i, c >>= k)
     | CCAMLreturnT (ty, v) ->
        k v >>= fun e ->
        CCAMLreturnT (Type_C.cexp e, e)
     | CCAMLreturn0 view ->
        CEff val_unit >>= fun u ->
        k (CCast (Typ.of_typ (View view), u)) >>= fun e ->
        CCAMLreturnT (Type_C.cexp e, e)
     | CLet (ye, c) ->
       (* let x = (let y = e1 in e2) in e3
          ~>
          let y = e1 in (let x = e2 in e3) *)
       CLet (ye, c >>= k)

  let (>>) c1 c2 = c1 >>= fun (_ : stmt cexp) -> c2

  let prim_prj : type a. a Primitives.prim -> (value -> a, a) cfunction =
    let open Primitives in function
    | Char -> reader "Int_val" (value @-> returning (Typ.of_typ char))
    | Schar -> reader "Int_val" (value @-> returning (Typ.of_typ int))
    | Uchar -> reader "Uint8_val" (value @-> returning (Typ.of_typ uchar))
    | Short -> reader "Int_val" (value @-> returning (Typ.of_typ int))
    | Int -> reader "Int_val" (value @-> returning (Typ.of_typ int))
    | Long -> reader "ctypes_long_val" (value @-> returning (Typ.of_typ long))
    | Llong -> reader "ctypes_llong_val" (value @-> returning (Typ.of_typ llong))
    | Ushort -> reader "ctypes_ushort_val" (value @-> returning (Typ.of_typ ushort))
    | Uint -> reader "ctypes_uint_val" (value @-> returning (Typ.of_typ uint))
    | Ulong -> reader "ctypes_ulong_val" (value @-> returning (Typ.of_typ ulong))
    | Ullong -> reader "ctypes_ullong_val" (value @-> returning (Typ.of_typ ullong))
    | Size_t -> reader "ctypes_size_t_val" (value @-> returning (Typ.of_typ size_t))
    | Int8_t -> reader "Int_val" (value @-> returning (Typ.of_typ int))
    | Int16_t -> reader "Int_val" (value @-> returning (Typ.of_typ int))
    | Int32_t -> reader "Int32_val" (value @-> returning (Typ.of_typ int32_t))
    | Int64_t -> reader "Int64_val" (value @-> returning (Typ.of_typ int64_t))
    | Uint8_t -> reader "Uint8_val" (value @-> returning (Typ.of_typ uint8_t))
    | Uint16_t -> reader "Uint16_val" (value @-> returning (Typ.of_typ uint16_t))
    | Uint32_t -> reader "Uint32_val" (value @-> returning (Typ.of_typ uint32_t))
    | Uint64_t -> reader "Uint64_val" (value @-> returning (Typ.of_typ uint64_t))
    | Camlint -> reader "Int_val" (value @-> returning (Typ.of_typ int))
    | Nativeint -> reader "Nativeint_val" (value @-> returning (Typ.of_typ nativeint))
    | Float -> reader "Double_val" (value @-> returning (Typ.of_typ double))
    | Double -> reader "Double_val" (value @-> returning (Typ.of_typ double))
    | Complex32 -> reader "ctypes_float_complex_val" (value @-> returning (Typ.of_typ complex32))
    | Complex64 -> reader "ctypes_double_complex_val" (value @-> returning (Typ.of_typ complex64))

  let prim_inj : type a. a Primitives.prim -> (a -> value, value) cfunction =
    let ty t = Typ.of_typ t in
    let open Primitives in function
    | Char -> immediater "Val_int" (ty char @-> returning value)
    | Schar -> immediater "Val_int" (ty int @-> returning value)
    | Uchar -> conser "ctypes_copy_uint8" (ty uchar @-> returning value)
    | Short -> immediater "Val_int" (ty int @-> returning value)
    | Int -> immediater "Val_int" (ty int @-> returning value)
    | Long -> conser "ctypes_copy_long" (ty long @-> returning value)
    | Llong -> conser "ctypes_copy_llong" (ty llong @-> returning value)
    | Ushort -> conser "ctypes_copy_ushort" (ty ushort @-> returning value)
    | Uint -> conser "ctypes_copy_uint" (ty uint @-> returning value)
    | Ulong -> conser "ctypes_copy_ulong" (ty ulong @-> returning value)
    | Ullong -> conser "ctypes_copy_ullong" (ty ullong @-> returning value)
    | Size_t -> conser "ctypes_copy_size_t" (ty size_t @-> returning value)
    | Int8_t -> immediater "Val_int" (ty int @-> returning value)
    | Int16_t -> immediater "Val_int" (ty int @-> returning value)
    | Int32_t -> conser "caml_copy_int32" (ty int32_t @-> returning value)
    | Int64_t -> conser "caml_copy_int64" (ty int64_t @-> returning value)
    | Uint8_t -> conser "ctypes_copy_uint8" (ty uint8_t @-> returning value)
    | Uint16_t -> conser "ctypes_copy_uint16" (ty uint16_t @-> returning value)
    | Uint32_t -> conser "ctypes_copy_uint32" (ty uint32_t @-> returning value)
    | Uint64_t -> conser "ctypes_copy_uint64" (ty uint64_t @-> returning value)
    | Camlint -> immediater "Val_int" (ty int @-> returning value)
    | Nativeint -> conser "caml_copy_nativeint" (ty nativeint @-> returning value)
    | Float -> conser "caml_copy_double" (ty double @-> returning value)
    | Double -> conser "caml_copy_double" (ty double @-> returning value)
    | Complex32 -> conser "ctypes_copy_float_complex" (ty complex32 @-> returning value)
    | Complex64 -> conser "ctypes_copy_double_complex" (ty complex64 @-> returning value)

  let app1 f x = CApp (f, Args (Arg x, End))
  let app2 f x y = CApp (f, Args (Arg x, Args (Arg y, End)))
  let app3 f x y z = CApp (f, Args (Arg x, Args (Arg y, Args (Arg z, End))))

  let of_fatptr : type a. a typ -> value cexp -> a ptr ceff =
    fun typ x ->
    app1 (reader "CTYPES_ADDR_OF_FATPTR" (value @-> returning (Typ.ptr typ))) x

  let string_to_ptr : value cexp -> char ptr ceff =
    fun x ->
    app1 (reader "CTYPES_PTR_OF_OCAML_STRING" (value @-> returning (Typ.ptr (Typ.of_typ char)))) x

  let float_array_to_ptr : value cexp -> float ptr ceff =
    fun x -> app1 (reader "CTYPES_PTR_OF_FLOAT_ARRAY"
                          (value @-> returning (Typ.ptr (Typ.of_typ double))))
                  x

  let from_ptr : type a. a typ -> a ptr cexp -> value ceff =
    fun typ x -> app1 (conser "CTYPES_FROM_PTR"
                              (Typ.ptr typ @-> returning value))
                      x

  let functions : value ptr ceff = CGlobal
    { name = "functions";
      references_ocaml_heap = true;
      typ = Typ.(ptr value) }

  let caml_callbackN n : (value -> int -> value carray -> value, value) cfunction =
    { fname = "caml_callbackN";
      allocates = true;
      reads_ocaml_heap = true;
      fn = Typ.(value @-> int @-> array n value @-> returning value) }

  let copy_bytes : type a. a typ -> (a ptr -> Unsigned.size_t -> value, value) cfunction =
    fun typ ->
    { fname = "ctypes_copy_bytes";
      allocates = true;
      reads_ocaml_heap = true;
      fn = Typ.(ptr typ @-> size_t @-> returning value) }

  let sizeof : type a. a typ -> Unsigned.Size_t.t cexp =
    fun typ -> CConst (CSizeof typ)

  let cast : type a b. into:b typ -> a ccomp -> b ccomp =
    fun ~into e -> e >>= fun x -> CEff (CExp (CCast (into, x)))

  type 'a prj_result = Prj of 'a ccomp
                     | OPrj : ('a, 'b) ocaml_pointer * 'b ccomp -> 'a prj_result
                     | NoPrj : ('a, unit) view -> 'a prj_result

  let id_view : type a. a typ -> (a, a) view =
    (* Here *)
    fun {ty_} -> let id x = x in
      { Static.read = id; write = id; ty=ty_; format = None; format_typ = None }
  let compose_view : type a b c. (a, b) view -> (b, c) view -> (a, c) view =
    fun v1 v2 ->
    let read x = v1.read (v2.read x) and write x = v2.write (v1.write x) in
    { Static.read; write; format = None; format_typ = None; ty = v2.Static.ty }

  let prj : type a. a typ -> value cexp -> a prj_result =
    fun ({format} as ty') ->
    let rec prj  : type a. a typ -> value cexp -> a prj_result =
    (* Here *)
    fun ({ty_} as ty') x -> match ty_ with
    | Void -> NoPrj (id_view (Typ.of_typ void))
    | Primitive p ->
      let { fn } as prj = prim_prj p in
      Prj (cast ~into:ty' (CEff (app1 prj x)))
    | Pointer t -> Prj (CEff (of_fatptr (Typ.of_typ t) x))
    | Struct s ->
      Prj (CEff (of_fatptr ty' x) >>= fun y -> CEff (CDeref y))
    | Union u -> 
      Prj (CEff (of_fatptr ty' x) >>= fun y -> CEff (CDeref y))
    | Abstract _ -> report_unpassable "values of abstract type"
    | View ({ Static.ty = ty'' } as view) -> 
       begin match prj (Typ.of_typ ty'') x with
       | Prj c -> Prj (c >>= fun y ->
                       CEff (CExp (CCast ({ty' with format}, y))))
       | OPrj _ -> assert false (* TODO *)
       | NoPrj v -> NoPrj (compose_view view v)
       end
    | Array _ -> report_unpassable "arrays"
    | Bigarray _ -> report_unpassable "bigarrays"
    | OCaml String -> OPrj (OString, CEff (string_to_ptr x))
    | OCaml Bytes -> OPrj (OBytes, CEff (string_to_ptr x))
    | OCaml FloatArray -> OPrj (OFloatArray, CEff (float_array_to_ptr x))
    in prj ty'

  let rec inj : type a. a typ -> a cexp -> value ceff =
    (* Here *)
    fun ({ty_} as ty') x -> match ty_ with
    | Void -> val_unit
    | Primitive p -> app1 (prim_inj p) (CCast (ty', x))
    | Pointer ty -> from_ptr (Typ.of_typ ty) x
    | Struct _ -> app2 (copy_bytes ty') (CAddr x) (sizeof ty')
    | Union _ -> app2 (copy_bytes ty') (CAddr x) (sizeof ty')
    | Abstract _ -> report_unpassable "values of abstract type"
    | View {Static.ty = ty'} -> inj (Typ.of_typ ty') (CCast (Typ.of_typ ty', x))
    | Array _ -> report_unpassable "arrays"
    | Bigarray _ -> report_unpassable "bigarrays"
    | OCaml _ -> report_unpassable "ocaml references as return values"

  let fundec : type a r. string -> (a, r) fn -> (a, r) cfundec =
    fun name fn -> Fundec (name, params fn, return_type fn)

  type (_, _) value_conversion =
      Value_returning : (_, value) value_conversion
    | Value_arg : ('r, 's) value_conversion -> ('a -> 'r, value -> 's) value_conversion

  type ('a, 'r) wrapper_function =
    Wrapper : ('a, 'r) fn * ('a, 'v) value_conversion * ('v, value) fn ->
              ('a, 'r) wrapper_function

  type 'a wrapper_dec =
    WrapperD : ('a, _) fn * ('a, 'v) value_conversion * ('v, value) fn * 'v cfundef ->
               'a wrapper_dec

  let rec value_wrapper : type a r. (a, r) fn -> (a, r) wrapper_function =
    fun fn -> match fn with
   | Returns_ _ -> Wrapper (fn, Value_returning, Returns_ value)
   | Function_ (p, fn') ->
      let Wrapper (f', vc, fv') = value_wrapper fn' in
      Wrapper (fn, Value_arg vc, Function_ (value, fv'))

  let fn : type a. cname:string -> stub_name:string -> a Static.fn -> a wrapper_dec =
    fun (type a) (type r) ~cname ~stub_name fn ->
    let Fn_wrapped fn = wrap_fn fn in
    let Wrapper (fn, vconv, value_fn) = value_wrapper fn in
      let fvar : (a, _) cfunction = { fname = cname;
                   allocates = false;
                   reads_ocaml_heap = false;
                   fn } in
      let rec body : type a r v.
       (a, r) fn (* The C function type *) ->
       (a, v) value_conversion (* The relation between the C type and the stub type *) ->
       v params (* The stub function type *) ->
       (a args -> r ccomp) -> 
       value ccomp
       = fun fn vconv params k ->
         match fn, vconv, params with
         | Returns_ t, Value_returning, NoParams ->
            k End >>= fun x ->
            CEff (inj t x)
         | Function_ (t, fn'), Value_arg vconv', Param (var, params') ->
            begin match prj t (CLocal var) with
                  | Prj c ->
                     c >>= fun x ->
                     let k' args = k (Args (Arg x, args)) in
                     body fn' vconv' params' k'
                  | OPrj (optr, c) -> 
                     c >>= fun x ->
                     let k' args = k (Args (OCamlArg (optr, x), args)) in
                     body fn' vconv' params' k'
                  | NoPrj v ->
                     let k' args = k (Args (Nothing v, args)) in
                     body fn' vconv' params' k'
            end
         | _ -> assert false (* internal error *)
      in
      let params = params value_fn in
      let stubdef = Fundef (Fundec (stub_name, params, value),
                            body fn vconv params
                                 (fun args -> CEff (CApp (fvar, args)))) in
      WrapperD (fn, vconv, value_fn, stubdef)

  let byte_fn : type v. bytename:string -> stubname:string ->
                     (v, value) fn -> (_, v) value_conversion ->
                     (value ptr -> int -> value) cfundef =
    fun ~bytename ~stubname:fname fn vconv ->
    let argv = local "argv" (Typ.ptr value) in
    let argc = local "argc" (Typ.of_typ int) in
    let f = { fname; allocates = true; reads_ocaml_heap = true; fn } in
    let rec body : type a v. (v, value) fn -> (a, v) value_conversion -> int -> 
                        (v args -> value ccomp) -> value ccomp =
     fun fn vconv i k -> match fn, vconv with
     | Returns_ _, Value_returning ->
        k End
     | Function_ (_, fn'), Value_arg vconv' ->
        CEff (CPIndex (CExp (CLocal argv), CConst (CInt i))) >>= fun (x : value cexp) ->
        let k' args = k (Args (Arg x, args)) in
        body fn' vconv' (i+1) k'
     | _ -> assert false
    in
    Fundef (Fundec (bytename, Param (argv, Param (argc, NoParams)), value),
            body fn vconv 0 (fun args -> CEff (CApp (f, args))))

  let inverse_fn : type a r. stub_name:string -> (a, r) wrapper_function ->
                             r typ -> a cfundef =
    fun ~stub_name (Wrapper (fn, vconv, vfn)) rtyp ->
    let idx = CLocal (local (Printf.sprintf "fn_%s" stub_name) (Typ.of_typ int)) in
    let params = params fn in
    let nparams = params_length params in
    let nargs = local "nargs" (Typ.of_typ int) in
    let locals = local "locals" (Typ.array nparams value) in
    let rec body : type a. a params -> int -> r ccomp =
     fun params i -> match params with
      | NoParams ->
         (* f := functions[fn_name];
            x := caml_callbackN(f, nargs, locals);
            y := T_val(x);
            CAMLreturnT(T, y);    *)
         CEff (CPIndex (functions, idx)) >>= fun f ->
         CEff (app3 (caml_callbackN nparams) f (CLocal nargs) (CLocal locals)) >>= fun x ->
         begin match prj rtyp x with
               | Prj c ->
                  c >>= fun y ->
                  CCAMLreturnT (rtyp, y)
               | OPrj _ -> assert false (* TODO *)
               | NoPrj v ->
                  CCAMLreturn0 v
         end
      | Param (var, params') ->
         (* locals[i] = Val_Ti(xi); *)
         CEff (CAssign (CPIndex_ (CVar (`Local (local "locals" (Typ.ptr value))),
                                  CConst (CInt i)), inj var.typ (CLocal var))) >>
         body params' (i + 1)
    in

      (* T f(T0 x0, T1 x1, ..., Tn xn) {
            enum { nargs = n };
            CAMLparam0();
            CAMLlocalN(locals, nargs);
            body
         }      *)
    Fundef (Fundec (stub_name, params, rtyp),
            CLetConst (nargs, CInt nparams,
                       CEff (CamlOp `CAMLparam0) >>
                       CEff (CamlOp (`CAMLlocalN (locals, CLocal nargs))) >>
                       body params 0))
end

let fn ~cname ~stub_name fmt fn =
  let Generate_C.WrapperD (fn2, vconv, value_fn,
                           (Fundef (Fundec (f, xs, _), _) as dec))
      = Generate_C.fn ~stub_name ~cname fn
  in
  let nargs = params_length xs in
  begin
    Cstubs_emit_c.cfundef fmt dec;
    if nargs > max_byte_args then
      let bytename = Printf.sprintf "%s_byte%d" f nargs in
      Cstubs_emit_c.cfundef fmt (Generate_C.byte_fn ~bytename ~stubname:f value_fn vconv)
  end

let inverse_fn ~stub_name fmt fn : unit =
  let Fn_wrapped fn = wrap_fn fn in
  let rtyp = return_type fn in
  Cstubs_emit_c.cfundef fmt
    (Generate_C.inverse_fn ~stub_name (Generate_C.value_wrapper fn) rtyp)

let inverse_fn_decl ~stub_name fmt fn =
  let Fn_wrapped fn = wrap_fn fn in
  Format.fprintf fmt "@[%a@];@\n"
    Cstubs_emit_c.cfundec (Generate_C.fundec stub_name fn)
