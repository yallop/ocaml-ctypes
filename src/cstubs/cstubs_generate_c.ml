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
type tfn = Typ : _ typ -> tfn | Fn : _ fn -> tfn

type id_properties = {
  name: string;
  allocates: bool;
  reads_ocaml_heap: bool;
  tfn: tfn;
}

type cvar = [ `Local of string * ty | `Global of id_properties ]
type cconst = [ `Int of int ]
type cexp = [ cconst
            | cvar
            | `Cast of ty * cexp
            | `Addr of cexp ]
type ccomp = [ cexp
             | `App of cvar * cexp list
             | `Return of cexp
             | `Let of (string * ty) * ccomp * ccomp
             | `Deref of cexp ]
type cfundec = [ `Function of string * string list * ccomp ]

let max_byte_args = 5

let rec return_type : type a. a fn -> ty = function
  | Function (_, f) -> return_type f
  | Returns t -> Ty t
                        
let rec typ_val : cexp -> ty = function
  | `Int _ -> Ty int
  | `Local (_, ty) -> ty
  | `Global { tfn = Typ t } -> Ty t
  | `Global { tfn = Fn f } -> internal_error
    "unexpected function type for expression: %s" (Ctypes.string_of_fn f)
  | `Cast (Ty ty, _) -> Ty ty
  | `Addr e -> let Ty ty = typ_val e in Ty (Pointer ty)

(* We're using an abstract type ([value]) as an argument and return type, so
   we'll use the [Function] and [Return] constructors directly.  The smart
   constructors [@->] and [returning] would reject the abstract type. *)
let (@->) f t = Function (f, t)
let returning t = Returns t

module Emit_C =
struct
  open Format

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
    fun ty e -> harmless ty (typ_val e)

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

  let rec ccomp env fmt : ccomp -> unit = function
    | #cexp as v -> cexp env fmt v
    | `App (v, es) ->
      fprintf fmt "@[%s(@[" (cvar_name v);
      let last_exp = List.length es - 1 in
      List.iteri
        (fun i e ->
          fprintf fmt "@[%a@]%(%)" (cexp env) e
            (if i <> last_exp then ",@ " else ""))
        es;
      fprintf fmt ")@]@]";
    | `Deref e -> fprintf fmt "@[*@[%a@]@]" (cexp env) e
    | `Return e -> fprintf fmt "@[<2>return@;@[%a@]@];" (cexp env) e
    | `Let ((x, _), (#cexp as v), e) ->
      (* substitute values during printing for now *)
      ccomp ((x, v) :: env) fmt e
    | `Let (x1, `Let (x2, c2, c3), c1) ->
      (* Normalise during printing for now *)
      ccomp env fmt (`Let (x2, c2, `Let (x1, c3, c1)))
    | `Let ((name, Ty Void), e, s) ->
      fprintf fmt "@[%a;@]@ %a" (ccomp env) e (ccomp env) s
    | `Let ((name, Ty (Struct { tag })), e, s) ->
      fprintf fmt "@[struct@;%s@;%s@;=@;@[%a;@]@]@ %a" tag name (ccomp env) e (ccomp env) s
    | `Let ((name, Ty (Union { utag })), e, s) ->
      fprintf fmt "@[union@;%s@;%s@;=@;@[%a;@]@]@ %a" utag name (ccomp env) e (ccomp env) s
    | `Let ((name, Ty ty), e, s) -> fprintf fmt "@[@[%a@]@;=@;@[%a;@]@]@ %a"
      (Ctypes.format_typ ~name) ty (ccomp env) e (ccomp env) s

  let cfundec fmt (`Function (f, xs, body) : cfundec) =
    fprintf fmt "@[value@;%s(@[" f;
    let xs_len = List.length xs - 1 in
    List.iteri
      (fun i x ->
        fprintf fmt "value %s%(%)" x
          (if i <> xs_len then ",@ " else ""))
      xs;
    fprintf fmt ")@]@]@\n{@[<v 2>@\n%a@]@\n}@\n" (ccomp []) body;
    let nargs = List.length xs in
    if nargs > max_byte_args then
      begin
        fprintf fmt "@[value@;%s_byte%d(value *argv, int argn)@]@\n{" f nargs;
        fprintf fmt "@[<v 2>@\nreturn@[@;%s(@[" f;
        for i = 0 to nargs - 1 do
          if i <> nargs - 1 then fprintf fmt "argv[%d],@ " i
          else fprintf fmt "argv[%d])@]@]@];@\n}@." i
        done
      end
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

  let from_ptr : cexp -> ccomp =
    fun x -> `App (`Global (conser "CTYPES_FROM_PTR" (ptr void @-> returning value)),
                   [x])

  let val_unit : ccomp = `Global { name = "Val_unit";
                                   allocates = false;
                                   reads_ocaml_heap = false;
                                   tfn = Typ value; }

  let copy_bytes : cvar = `Global { name = "ctypes_copy_bytes";
                                    allocates = true;
                                    reads_ocaml_heap = true;
                                    tfn = Fn (ptr void @-> size_t @-> returning value) }

  let cast : type a b. from:ty -> into:ty -> ccomp -> ccomp =
    fun ~from ~into e ->
      let x = (fresh_var (), from) in
      `Let (x, e, `Cast (into, `Local x))

  let rec prj : type a. a typ -> cexp -> ccomp option =
    fun ty x -> match ty with
    | Void -> None
    | Primitive p ->
      begin match prim_prj p with
      | { tfn = Fn fn } as prj ->
        let rt = return_type fn in
        Some (cast ~from:rt ~into:(Ty (Primitive p)) (`App (`Global prj, [x])))
      | { tfn = Typ t } -> internal_error
        "Unexpected non-function type for function: %s" (Ctypes.string_of_typ t)
      end
    | Pointer _ -> Some (to_ptr x)
    | Struct s ->
      let y = (fresh_var (), Ty (ptr void)) in
      Some (`Let (y, to_ptr x,
                  `Deref (`Cast (Ty (ptr ty), `Local y))))
    | Union u -> 
      let y = (fresh_var (), Ty (ptr void)) in
      Some (`Let (y, to_ptr x,
                  `Deref (`Cast (Ty (ptr ty), `Local y))))
    | Abstract _ -> report_unpassable "values of abstract type"
    | View { ty } -> prj ty x
    | Array _ -> report_unpassable "arrays"
    | Bigarray _ -> report_unpassable "bigarrays"

  let rec inj : type a. a typ -> cexp -> ccomp =
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
      
  type ('state, 'result) folder = {
    arg: 'a. 'a typ -> 'state -> 'state;
    returns: 'a. 'a typ -> 'state -> 'result;
  }

  let rec foldl_fn : type a s r. (s, r) folder -> s -> a fn -> r =
    fun ({ arg; returns } as folder) init -> function 
    | Returns t -> returns t init
    | Function (f, t) -> foldl_fn folder (arg f init) t

  type fold_state = {
    env: cvar list;
    body: ccomp -> ccomp;
    vars: string list;
  }

  let fn : type a. cname:string -> stub_name:string -> a fn -> cfundec =
    fun ~cname ~stub_name f ->
      foldl_fn {
        arg = (fun ty state ->
          let x = fresh_var () in
          let x' = fresh_var () in 
          match prj ty (`Local (x, Ty value)) with
          | None ->
            { state with vars = x :: state.vars }
          | Some projected ->
            { vars = x :: state.vars;
              body = (fun e -> state.body (`Let ((x', Ty ty), projected, e)));
              env = `Local (x', Ty ty) :: state.env });
        returns = (fun t state ->
          let x = fresh_var () in
          `Function (stub_name, List.rev state.vars,
                     state.body (`Let ((x, Ty t),
                                       (`App
                                           (`Global { name = cname;
                                                      allocates = false;
                                                      reads_ocaml_heap = false;
                                                      tfn = Fn f; },
                                            (List.rev state.env :> cexp list))),
                                       let y = (fresh_var (), Ty value) in
                                       `Let (y, inj t (`Local (x, Ty t)),
                                             `Return (`Local y))))))}
          { env = []; body = (fun e -> e); vars = [] } f
end

let fn ~cname  ~stub_name fmt fn =
  Emit_C.cfundec fmt (Generate_C.fn ~stub_name ~cname fn)
