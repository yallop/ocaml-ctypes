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

type value_
type value = value_ abstract
let value : value typ = abstract ~name:"value" ~size:0 ~alignment:0

type stmt_
type stmt = stmt_ abstract
let stmt : stmt typ = abstract ~name:"stmt" ~size:0 ~alignment:0

type (_, _) fn =
    Returns_  : 'r typ  -> ('r, 'r) fn
  | Function_ : 'a typ * ('b, 'r) fn  -> ('a -> 'b, 'r) fn

type 'a fn_wrapper = Fn_wrapped : ('a, 'r) fn -> 'a fn_wrapper

let rec wrap_fn : type a. a Static.fn -> a fn_wrapper =
 fun fn -> match fn with
   Returns ty  -> Fn_wrapped (Returns_ ty)
 | Function (p, fn') ->
    let (Fn_wrapped fn'') = wrap_fn fn' in
    Fn_wrapped (Function_ (p, fn''))

type ('a, 'r) cfunction = {
  fname: string;
  allocates: bool;
  reads_ocaml_heap: bool;
  fn: ('a, 'r) fn;
}

type 'a cvar_details = {
  name: string;
  typ: 'a typ;
  references_ocaml_heap: bool;
}

type 'a clocal = [ `Local of 'a cvar_details ]
type 'a cglobal = [ `Global of 'a cvar_details ]
type 'a cvar = [ 'a clocal | 'a cglobal ]
type _ cconst =
    CInt : int -> int cconst
  | CSizeof : _ typ -> Unsigned.size_t cconst
type 'a cexp = CConst of 'a cconst
             | CLocal of 'a cvar_details
             | CCast : 'a typ * _ cexp -> 'a cexp
             | CAddr : 'a cexp -> 'a ptr cexp
type 'a clvalue =
    CVar of 'a cvar
  | CAIndex_ : 'a carray clvalue * int cexp -> 'a clvalue
  | CPIndex_ : 'a ptr clvalue * int cexp -> 'a clvalue
type camlop = [ `CAMLparam0
              | `CAMLlocalN of value carray cvar_details * int cexp ]

type (_, _) ocaml_pointer =
    OString : (string ocaml, char ptr) ocaml_pointer
  | OBytes : (Bytes.t ocaml, char ptr) ocaml_pointer
  | OFloatArray : (float array ocaml, float ptr) ocaml_pointer

type 'a arg =
    Arg of 'a cexp
  | Nothing of ('a, unit) view
  | OCamlArg : ('a, 'o) ocaml_pointer * 'o cexp -> 'a arg

type 'a args =
    End
  | Args : 'a arg * 'b args -> ('a -> 'b) args

type 'a ceff =
    CExp of 'a cexp
  | CamlOp : camlop -> stmt ceff
  | CGlobal of 'a cvar_details
  | CApp : ('a, 'r) cfunction * 'a args -> 'r ceff
  | CIf of bool cexp * 'a ceff * 'a ceff
  | CPIndex of 'a ptr ceff * int cexp
  | CAIndex of 'a carray ceff * int cexp
  | CDeref of 'a ptr cexp
  | CAssign : 'a clvalue * 'a ceff -> stmt ceff
type 'a cbind = CBind of 'a cvar_details * 'a ceff
type 'a ccomp = (* This isn't quite right: it doesn't handle the return type properly *)
    CEff of 'a ceff
  | CLetConst : _ cvar_details * _ cconst * 'a ccomp -> 'a ccomp
  | CCAMLreturnT of 'a typ * 'a cexp
  | CCAMLreturn0 of ('a, unit) view
  | CLet : _ cbind * 'a ccomp -> 'a ccomp
type _ params =
    NoParams
  | Param : 'a cvar_details * 'b params -> ('a -> 'b) params
type ('a, 'r) cfundec = Fundec of string * 'a params * 'r typ
type 'a cfundef = Fundef : ('a, 'r) cfundec * 'r ccomp -> 'a cfundef

let rec args_length : type a. a args -> int = function
 | End -> 0
 | Args (Nothing _, xs) -> args_length xs
 | Args ((OCamlArg _| Arg _), xs) -> 1 + args_length xs

let rec params_length : type a. a params -> int = function
    NoParams -> 0
  | Param (_, p) -> 1 + params_length p

let rec return_type : type a r. (a, r) fn -> r typ = function
    Returns_ t -> t
  | Function_ (_, f) -> return_type f

let params : type a r. (a, r) fn -> a params = fun fn ->
  let rec loop : type a. (a, r) fn -> a params = function
      Returns_ _ -> NoParams
    | Function_ (typ, fn) ->
       Param ({name=fresh_var (); references_ocaml_heap=true; typ}, loop fn)
  in loop fn

type iteri_arg = { argf : 'a. int -> 'a cexp -> unit }

let iteri_args : type a. f:iteri_arg -> a args -> unit =
  fun ~f:{argf=f} args ->
  let rec loop : type a. int -> a args -> unit =
    fun i -> function
   | End -> ()
   | Args (Arg x, xs) -> f i x; loop (i+1) xs
   | Args (OCamlArg (_, x), xs) -> f i x; loop (i+1) xs
   | Args (Nothing _, xs) -> loop (i+1) xs
 in loop 0 args

type iteri_param = { paramf : 'a. int -> 'a cvar_details -> unit }

let rec iteri_params : type a. f:iteri_param -> a params -> unit =
  fun ~f:{paramf=f} params ->
  let rec loop : type a r. int -> a params -> unit =
    fun i -> function
   | NoParams -> ()
   | Param (x, xs) -> f i x; loop (i+1) xs
 in loop 0 params

module Type_C =
struct
  let cconst : type a. a cconst -> a typ = function
      CInt _ -> int
    | CSizeof _ -> size_t

  let rec cexp : type a. a cexp -> a typ = function
      CConst c -> cconst c
    | CLocal {typ} -> typ
    | CCast (typ, _) -> typ
    | CAddr e -> ptr (cexp e)

  let rec ceff : type a. a ceff -> a typ = function
      CExp e -> cexp e
    | CamlOp o -> stmt
    | CGlobal { typ } -> typ
    | CApp ({fn}, _) -> return_type fn
    | CIf (_, e, _) -> ceff e
    | CAIndex (e, _) -> array_reference_ceff e
    | CPIndex (e, _) -> ptr_reference_ceff e
    | CDeref e -> ptr_reference_ceff (CExp e)
    | CAssign (_, rv) -> stmt
  and ptr_reference_ceff : type a. a ptr ceff -> a typ =
    fun e ->
      begin match ceff e with
        Pointer ty -> ty
      | t -> Cstubs_errors.internal_error
        "dereferencing expression of non-pointer type %s"
        (Ctypes.string_of_typ t)
      end
  and array_reference_ceff : type a. a carray ceff -> a typ =
    fun e ->
      begin match ceff e with
        Array (ty, _) -> ty
      | t -> Cstubs_errors.internal_error
        "dereferencing expression of non-array type %s"
        (Ctypes.string_of_typ t)
      end
end
