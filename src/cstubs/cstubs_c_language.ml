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
