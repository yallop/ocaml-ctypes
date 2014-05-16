(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* C stub generation *)

open Static
open Cstubs_errors

type ty = Ty : _ typ -> ty
type _ tfn =
  Typ : _ typ -> [`Typ] tfn
| Fn : _ fn -> [`Fn] tfn

type 'a id_properties = {
  name: string;
  allocates: bool;
  reads_ocaml_heap: bool;
  tfn: 'a tfn;
}

type 'a cglobal = [ `Global of 'a id_properties ]
type clocal = [ `Local of string * ty ]
type cvar = [ clocal | [`Typ] cglobal ]
type cconst = [ `Int of int ]
type cexp = [ cconst
            | cvar
            | `Cast of ty * cexp
            | `Addr of cexp ]
type clvalue = [ clocal | `Index of clvalue * cexp ]
type ceff = [ cexp
            | `App of [`Fn] cglobal * cexp list
            | `Index of cexp * cexp
            | `Deref of cexp
            | `Assign of clvalue * ceff ]
type cbind = clocal * ceff
type ccomp = [ ceff
             | `Let of cbind * ccomp ]
type cfundec = [ `Fundec of string * (string * ty) list * ty ]
type cfundef = [ `Function of cfundec * ccomp ]

let max_byte_args = 5

let rec return_type : type a. a fn -> ty = function
  | Function (_, f) -> return_type f
  | Returns t -> Ty t
                        
module Type_C =
struct
  let rec cexp : cexp -> ty = function
    | `Int _ -> Ty int
    | `Local (_, ty) -> ty
    | `Global { tfn = Typ t } -> Ty t
    | `Cast (Ty ty, _) -> Ty ty
    | `Addr e -> let Ty ty = cexp e in Ty (Pointer ty)

  let rec ceff : ceff -> ty = function
    | #cexp as e -> cexp e
    | `App (`Global  { tfn = Fn f; name }, _) -> return_type f
    | `Index (e, _)
    | `Deref e ->
      begin match cexp e with
      | Ty (Pointer ty) -> Ty ty
      | Ty (Array (ty, _)) -> Ty ty
      | Ty t -> internal_error
        "dereferencing expression of non-pointer type %s"
        (Ctypes.string_of_typ t)
      end
    | `Assign (_, rv) -> ceff rv

  let rec ccomp : ccomp -> ty = function
    | #cexp as e -> cexp e
    | #ceff as e -> ceff e
    | `Let (_, c) -> ccomp c
end

(* We're using an abstract type ([value]) as an argument and return type, so
   we'll use the [Function] and [Return] constructors directly.  The smart
   constructors [@->] and [returning] would reject the abstract type. *)
let (@->) f t = Function (f, t)
let returning t = Returns t

module Emit_C =
struct
  open Format

  let format_seq lbr fmt_item sep rbr fmt items =
    let open Format in
    fprintf fmt "%s@[@[" lbr;
      ListLabels.iteri items ~f:(fun i item ->
        if i <> 0 then fprintf fmt "@]%s@ @[" sep;
        fmt_item fmt item);
    fprintf fmt "@]%s@]" rbr

  let format_ty fmt (Ty ty) = Ctypes.format_typ fmt ty

  let cvar_name = function
    | `Local (name, _) | `Global { name } -> name

  let cvar fmt v = fprintf fmt "%s" (cvar_name v)

  let cconst fmt (`Int i) = fprintf fmt "%d" i

  (* Determine whether the C expression [(ty)e] is equivalent to [e] *)
  let cast_unnecessary : ty -> cexp -> bool =
    let rec harmless l r = match l, r with
    | Ty (Pointer Void), Ty (Pointer _) -> true
    | Ty (View { ty }), t -> harmless (Ty ty) t
    | t, Ty (View { ty }) -> harmless t (Ty ty)
    | (Ty (Primitive _) as l), (Ty (Primitive _) as r) -> l = r
    | _ -> false
    in
    fun ty e -> harmless ty (Type_C.cexp e)

  let rec cexp env fmt : cexp -> unit = function
    | #cconst as c -> cconst fmt c
    | `Local (y, _) as x ->
      begin
        try cexp env fmt (List.assoc y env)
        with Not_found -> cvar fmt x
      end
    | #cvar as x -> cvar fmt x
    | `Cast (ty, e) when cast_unnecessary ty e -> cexp env fmt e
    | `Cast (ty, e) -> fprintf fmt "@[@[(%a)@]%a@]" format_ty ty (cexp env) e
    | `Addr e -> fprintf fmt "@[&@[%a@]@]" (cexp env) e

  let rec clvalue env fmt : clvalue -> unit = function
    | `Local _ as x -> cvar fmt x
    | `Index (lv, i) ->
      fprintf fmt "@[@[%a@]@[[%a]@]@]" (clvalue env) lv (cexp env) i

  let rec ceff env fmt : ceff -> unit = function
    | #cexp as e -> cexp env fmt e
    | `App (v, es) ->
      fprintf fmt "@[%s(@[" (cvar_name v);
      let last_exp = List.length es - 1 in
      List.iteri
        (fun i e ->
          fprintf fmt "@[%a@]%(%)" (cexp env) e
            (if i <> last_exp then ",@ " else ""))
        es;
      fprintf fmt ")@]@]";
    | `Index (e, i) ->
      fprintf fmt "@[@[%a@]@[[%a]@]@]"
        (cexp env) e (cexp env) i
    | `Deref e -> fprintf fmt "@[*@[%a@]@]" (cexp env) e
    | `Assign (lv, e) ->
      fprintf fmt "@[@[%a@]@;=@;@[%a@]@]"
        (clvalue env) lv (ceff env) e

  let rec ccomp env fmt : ccomp -> unit = function
    | #cexp as e -> fprintf fmt "@[<2>return@;@[%a@]@];" (cexp env) e
    | #ceff as e -> fprintf fmt "@[<2>return@;@[%a@]@];" (ceff env) e
    | `Let (xe, `Cast (ty, (#cexp as e'))) when cast_unnecessary ty e' ->
      ccomp env fmt (`Let (xe, e'))
    | `Let ((`Local (x, _), e), `Local (y, _)) when x = y ->
      ccomp env fmt (e :> ccomp)
    | `Let ((`Local (name, Ty Void), e), s) ->
      fprintf fmt "@[%a;@]@ %a" (ceff env) e (ccomp env) s
    | `Let ((`Local (name, Ty (Struct { tag })), e), s) ->
      fprintf fmt "@[struct@;%s@;%s@;=@;@[%a;@]@]@ %a"
        tag name (ceff env) e (ccomp env) s
    | `Let ((`Local (name, Ty (Union { utag })), e), s) ->
      fprintf fmt "@[union@;%s@;%s@;=@;@[%a;@]@]@ %a"
        utag name (ceff env) e (ccomp env) s
    | `Let ((`Local (name, Ty ty), e), s) ->
      fprintf fmt "@[@[%a@]@;=@;@[%a;@]@]@ %a"
        (Ctypes.format_typ ~name) ty (ceff env) e (ccomp env) s

  let cfundef fmt (`Function (`Fundec (f, xs, _), body) : cfundef) =
    fprintf fmt "@[value@;%s(@[" f;
    let xs_len = List.length xs - 1 in
    List.iteri
      (fun i (name, Ty ty) ->
        fprintf fmt "%a%(%)"
          (Type_printing.format_typ ~name) ty
          (if i <> xs_len then ",@ " else ""))
      xs;
    fprintf fmt ")@]@]@\n{@[<v 2>@\n%a@]@\n}@\n" (ccomp []) body
end

let value = abstract ~name:"value" ~size:0 ~alignment:0

module Generate_C =
struct
  let var_counter = ref 0
  let fresh_var () =
    incr var_counter;
    Printf.sprintf "x%d" !var_counter

  let report_unpassable what =
    let msg = Printf.sprintf "cstubs does not support passing %s" what in
    raise (Unsupported msg)

  let reader name fn = { name; allocates = false; reads_ocaml_heap = true; tfn = Fn fn }
  let conser name fn = { name; allocates = true; reads_ocaml_heap = false; tfn = Fn fn }
  let immediater name fn = { name; allocates = false; reads_ocaml_heap = false; tfn = Fn fn }

  let rec (>>=) : type a. ccomp * a typ -> (cexp -> ccomp) -> ccomp =
   fun (e, ty) k ->
     let x = fresh_var () in
     match e with
       (* let x = v in e ~> e[x:=v] *) 
     | #cexp as v ->
       k v
     | #ceff as e ->
       `Let ((`Local (x, Ty ty), e), k (`Local (x, Ty ty)))
     | `Let (ye, c) ->
       (* let x = (let y = e1 in e2) in e3
          ~>
          let y = e1 in (let x = e2 in e3) *)
       let Ty t = Type_C.ccomp c in
       `Let (ye, (c, t) >>= k)

  let prim_prj : type a. a Primitives.prim -> _ =
    let open Primitives in function
    | Char -> reader "Int_val" (value @-> returning int)
    | Schar -> reader "Int_val" (value @-> returning int)
    | Uchar -> reader "Uint8_val" (value @-> returning uint8_t)
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

  let prim_inj : type a. a Primitives.prim -> _ =
    let open Primitives in function
    | Char -> immediater "Val_int" (int @-> returning value)
    | Schar -> immediater "Val_int" (int @-> returning value)
    | Uchar -> conser "ctypes_copy_uint8" (uint8_t @-> returning value)
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

  let to_ptr : cexp -> ccomp =
    fun x -> `App (`Global (reader "CTYPES_TO_PTR" (value @-> returning (ptr void))),
                   [x])

  let string_to_ptr : cexp -> ccomp =
    fun x -> `App (`Global (reader "CTYPES_PTR_OF_OCAML_STRING"
                              (value @-> returning (ptr void))),
                   [x])

  let float_array_to_ptr : cexp -> ccomp =
    fun x -> `App (`Global (reader "CTYPES_PTR_OF_FLOAT_ARRAY"
                              (value @-> returning (ptr void))),
                   [x])

  let from_ptr : cexp -> ceff =
    fun x -> `App (`Global (conser "CTYPES_FROM_PTR" (ptr void @-> returning value)),
                   [x])

  let val_unit : ceff = `Global { name = "Val_unit";
                                  allocates = false;
                                  reads_ocaml_heap = false;
                                  tfn = Typ value; }

  let copy_bytes : [`Fn] cglobal =
    `Global { name = "ctypes_copy_bytes";
              allocates = true;
              reads_ocaml_heap = true;
              tfn = Fn (ptr void @-> size_t @-> returning value) }

  let cast : type a b. from:ty -> into:ty -> ccomp -> ccomp =
    fun ~from:(Ty from) ~into e ->
      (e, from) >>= fun x ->
      `Cast (into, x)

  let rec prj : type a. a typ -> cexp -> ccomp option =
    fun ty x -> match ty with
    | Void -> None
    | Primitive p ->
      let { tfn = Fn fn } as prj = prim_prj p in
      let rt = return_type fn in
      Some (cast ~from:rt ~into:(Ty (Primitive p)) (`App (`Global prj, [x])))
    | Pointer _ -> Some (to_ptr x)
    | Struct s ->
      Some ((to_ptr x, ptr void) >>= fun y ->
            `Deref (`Cast (Ty (ptr ty), y)))
    | Union u -> 
      Some ((to_ptr x, ptr void) >>= fun y ->
            `Deref (`Cast (Ty (ptr ty), y)))
    | Abstract _ -> report_unpassable "values of abstract type"
    | View { ty } -> prj ty x
    | Array _ -> report_unpassable "arrays"
    | Bigarray _ -> report_unpassable "bigarrays"
    | OCaml String -> Some (string_to_ptr x)
    | OCaml FloatArray -> Some (float_array_to_ptr x)

  let rec inj : type a. a typ -> cexp -> ceff =
    fun ty x -> match ty with
    | Void -> val_unit
    | Primitive p -> `App (`Global (prim_inj p), [`Cast (Ty (Primitive p), x)])
    | Pointer _ -> from_ptr x
    | Struct s -> `App (copy_bytes, [`Addr x; `Int (sizeof ty)])
    | Union u -> `App (copy_bytes, [`Addr x; `Int (sizeof ty)])
    | Abstract _ -> report_unpassable "values of abstract type"
    | View { ty } -> inj ty x
    | Array _ -> report_unpassable "arrays"
    | Bigarray _ -> report_unpassable "bigarrays"
    | OCaml _ -> report_unpassable "ocaml references as return values"

  type _ fn =
  | Returns  : 'a typ   -> 'a fn
  | Function : string * 'a typ * 'b fn  -> ('a -> 'b) fn

  let rec name_params : type a. a Static.fn -> a fn = function
    | Returns t -> Returns t
    | Function (f, t) -> Function (fresh_var (), f, name_params t)

  let rec value_params : type a. a fn -> (string * ty) list = function
    | Returns t -> []
    | Function (x, _, t) -> (x, Ty value) :: value_params t

  let fn : type a. cname:string -> stub_name:string -> a Static.fn -> cfundef =
    fun ~cname ~stub_name f ->
      let fvar = `Global { name = cname;
                           allocates = false;
                           reads_ocaml_heap = false;
                           tfn = Fn f; } in
      let rec body : type a. _ -> a fn -> _ =
         fun vars -> function 
         | Returns t ->
           (`App (fvar, (List.rev vars :> cexp list)), t) >>= fun x ->
           (inj t x :> ccomp)
         | Function (x, f, t) ->
           begin match prj f (`Local (x, Ty value)) with
             None -> body vars t
           | Some projected -> 
             (projected, f) >>= fun x' ->
             body (x' :: vars) t
           end
      in
      let f' = name_params f in
      `Function (`Fundec (stub_name, value_params f', Ty value),
                 body [] f')

  let byte_fn : type a. string -> a Static.fn -> int -> cfundef =
    fun name fn nargs ->
      let argv = ("argv", Ty (ptr value)) in
      let argc = ("argc", Ty int) in
      let f = `Global { name ;
                        allocates = true;
                        reads_ocaml_heap = true;
                        tfn = Fn fn }
      in
      let rec build_call ?(args=[]) = function
        | 0 -> `App (f, args)
        | n -> (`Index (`Local argv, `Int (n - 1)), value) >>= fun x ->
               build_call ~args:(x :: args) (n - 1)
      in
      let bytename = Printf.sprintf "%s_byte%d" name nargs in
      `Function (`Fundec (bytename, [argv; argc], Ty value),
                 build_call nargs)
end

let fn ~cname  ~stub_name fmt fn =
  let `Function (`Fundec (f, xs, _), _) as dec
      = Generate_C.fn ~stub_name ~cname fn
  in
  let nargs = List.length xs in
  if nargs > max_byte_args then begin
    Emit_C.cfundef fmt dec;
    Emit_C.cfundef fmt (Generate_C.byte_fn f fn nargs)
  end
  else
    Emit_C.cfundef fmt dec
