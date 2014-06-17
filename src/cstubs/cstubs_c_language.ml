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
type tfn = Fn : _ fn -> tfn

type cfunction = {
  fname: string;
  allocates: bool;
  reads_ocaml_heap: bool;
  fn: tfn;
}

type cglobal = {
  name: string;
  typ: ty;
  references_ocaml_heap: bool;
}

type clocal = [ `Local of string * ty ]
type cvar = [ clocal | `Global of cglobal ]
type cconst = [ `Int of int ]
type cexp = [ cconst
            | clocal
            | `Cast of ty * cexp
            | `Addr of cexp ]
type clvalue = [ clocal | `Index of clvalue * cexp ]
type camlop = [ `CAMLparam0
              | `CAMLlocalN of cexp * cexp ]
type ceff = [ cexp
            | camlop
            | `Global of cglobal
            | `App of cfunction * cexp list
            | `If of cexp * ceff * ceff
            | `Index of ceff * cexp
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
    | `Cast (Ty ty, _) -> Ty ty
    | `Addr e -> let Ty ty = cexp e in Ty (Pointer ty)

  let camlop : camlop -> ty = function
    | `CAMLparam0
    | `CAMLlocalN _ -> Ty Void

  let rec ceff : ceff -> ty = function
    | #cexp as e -> cexp e
    | #camlop as o -> camlop o
    | `Global { typ } -> typ
    | `App ({ fn = Fn f }, _) -> return_type f
    | `If (_, e, _) -> ceff e
    | `Index (e, _) -> reference_ceff e
    | `Deref e -> reference_ceff (e :> ceff)
    | `Assign (_, rv) -> ceff rv
  and reference_ceff : ceff -> ty =
    fun e ->
      begin match ceff e with
      | Ty (Pointer ty) -> Ty ty
      | Ty (Array (ty, _)) -> Ty ty
      | Ty t -> Cstubs_errors.internal_error
        "dereferencing expression of non-pointer type %s"
        (Ctypes.string_of_typ t)
      end

  let rec ccomp : ccomp -> ty = function
    | #cexp as e -> cexp e
    | #ceff as e -> ceff e
    | `Let (_, c)
    | `LetConst (_, _, c) -> ccomp c
    | `CAMLreturnT (ty, _) -> ty
end
