(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* C code representation. *)

open Static

let fresh_var =
  let var_counter = ref 0 in
  fun () ->
    incr var_counter;
    Printf.sprintf "x%d" !var_counter

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
type camlop = [ `CAMLparam0
              | `CAMLlocalN of cexp * cexp ]
type ceff = [ cexp
            | camlop
            | `App of [`Fn] cglobal * cexp list
            | `Index of cexp * cexp
            | `Deref of cexp
            | `Assign of clvalue * ceff ]
type cbind = clocal * ceff
type ccomp = [ ceff
             | `LetConst of clocal * cconst * ccomp
             | `CAMLreturnT of ty * cexp
             | `Let of cbind * ccomp ]
type cfundec = [ `Fundec of string * (string * ty) list * ty ]
type cfundef = [ `Function of cfundec * ccomp ]

let rec return_type : type a. a fn -> ty = function
  | Function (_, f) -> return_type f
  | Returns t -> Ty t

let args : type a. a fn -> (string * ty) list = fun fn ->
  let rec loop : type a. a Ctypes.fn -> (string * ty) list = function
    | Static.Function (ty, fn) -> (fresh_var (), Ty ty) :: loop fn
    | Static.Returns _ -> []
  in loop fn

module Type_C =
struct
  let rec cexp : cexp -> ty = function
    | `Int _ -> Ty int
    | `Local (_, ty) -> ty
    | `Global { tfn = Typ t } -> Ty t
    | `Cast (Ty ty, _) -> Ty ty
    | `Addr e -> let Ty ty = cexp e in Ty (Pointer ty)

  let camlop : camlop -> ty = function
    | `CAMLparam0
    | `CAMLlocalN _ -> Ty Void

  let rec ceff : ceff -> ty = function
    | #cexp as e -> cexp e
    | #camlop as o -> camlop o
    | `App (`Global  { tfn = Fn f; name }, _) -> return_type f
    | `Index (e, _)
    | `Deref e ->
      begin match cexp e with
      | Ty (Pointer ty) -> Ty ty
      | Ty (Array (ty, _)) -> Ty ty
      | Ty t -> Cstubs_errors.internal_error
        "dereferencing expression of non-pointer type %s"
        (Ctypes.string_of_typ t)
      end
    | `Assign (_, rv) -> ceff rv

  let rec ccomp : ccomp -> ty = function
    | #cexp as e -> cexp e
    | #ceff as e -> ceff e
    | `Let (_, c)
    | `LetConst (_, _, c) -> ccomp c
    | `CAMLreturnT (ty, _) -> ty
end
