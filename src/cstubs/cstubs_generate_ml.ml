(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* ML stub generation *)

open Static
open Ctypes_path
open Cstubs_errors

type lident = string
type ml_type = [ `Ident of path
	       | `Appl of path * ml_type list
	       | `Fn of ml_type * ml_type ]

type ml_external_type = [ `Prim of ml_type list * ml_type ]

type ty = Ty : 'a typ -> ty

type ml_exp = [ `Ident of path 
              | `Project of ml_exp * path
              | `MakePtr of ty * ml_exp
              | `MakeStructured of ty * ml_exp
              | `Coerce of ty * ty * ml_exp
              | `Appl of ml_exp * ml_exp ]
type ml_dec = [ `Function of lident * ml_type * lident list * ml_exp ]

type attributes = { float: bool; noalloc: bool }

type extern = {
  ident : string;
  typ: ml_external_type;
  primname: string;
  primname_byte: string option;
  attributes: attributes;
}

module Emit_ML =
struct
  let fprintf, pp_print_string = Format.(fprintf, pp_print_string)

  (* We (only) need to parenthesize function types in certain contexts 
        * on the lhs of a function type: - -> t
        * as the argument to a single-argument type constructor: - t
  *)
  type arrow_parens = ArrowParens | NoArrowParens

  (* We (only) need to parenthesize application expressions in certain contexts 
        * in a projection expression: -.l
        * in a dereference expression: !@ -
        * as an argument in an application: e -
  *)
  type appl_parens = ApplParens | NoApplParens

  let user_type fmt ty =
    format_path fmt (Cstubs_public_name.((retrieve_path ty).value))

  let rec typ : type a. appl_parens -> Format.formatter -> a Static.typ -> unit =
    let open Format in
    fun appl_parens fmt ty -> match appl_parens, ty with
    | _, Void -> fprintf fmt "Ctypes.void"
    | _, Primitive p ->
      format_path fmt (Cstubs_public_name.constructor_ident_of_prim p)
    | ApplParens, Pointer t -> fprintf fmt "@[(Ctypes.ptr@ @[%a@])@]" (typ ApplParens) t
    | NoApplParens, Pointer t -> fprintf fmt "@[Ctypes.ptr@ @[%a@]@]" (typ ApplParens) t
    | ApplParens, Array (t, n) -> fprintf fmt "@[(array@ %d@ @[%a@])@]" n (typ ApplParens) t
    | NoApplParens, Array (t, n) -> fprintf fmt "@[array@ %d@ @[%a@]@]" n (typ ApplParens) t
    | _, Bigarray _ -> failwith "Not yet implemented"
    | _, Struct _ -> user_type fmt ty
    | _, Union _ -> user_type fmt ty
    | _, Abstract _ -> user_type fmt ty
    | _, View _ -> user_type fmt ty

  let rec fn : type a. appl_parens -> Format.formatter -> a Static.fn -> unit =
    let open Format in
    fun appl_parens fmt f -> match appl_parens, f with
    | ApplParens, Function (a, b) -> fprintf fmt "@[(%a@ @@->@ %a)@]" (typ NoApplParens) a (fn NoApplParens) b
    | NoApplParens, Function (a, b) -> fprintf fmt "@[%a@ @@->@ %a@]" (typ NoApplParens) a (fn NoApplParens) b
    | ApplParens, Returns t -> fprintf fmt "@[(returning@ @[%a@])@}" (typ ApplParens) t
    | NoApplParens, Returns t -> fprintf fmt "@[returning@ @[%a@]@]" (typ ApplParens) t

  let ident = format_path

  let rec ml_type arrow_parens fmt t =
    match arrow_parens, t with
    | _, `Ident i -> ident fmt i
    | _, `Appl (t, []) -> ident fmt t
    | _, `Appl (t, [t']) ->
      fprintf fmt "@[%a@ %a@]" (ml_type ArrowParens) t' ident t
    | _, `Appl (t, ts) ->
      let nargs = List.length ts in
      fprintf fmt "(";
      List.iteri
        (fun i arg ->
          if i = nargs - 1 then (ml_type NoArrowParens) fmt arg
          else fprintf fmt "%a,@ " (ml_type NoArrowParens) arg
        ) ts;
      fprintf fmt ")@ %a" ident t;
    | ArrowParens, `Fn (t, t') ->
      fprintf fmt "@[(%a@ ->@ %a)@]" (ml_type ArrowParens) t (ml_type NoArrowParens) t'
    | NoArrowParens, `Fn (t, t') ->
      fprintf fmt "@[%a@ ->@]@ %a" (ml_type ArrowParens) t (ml_type NoArrowParens) t'

  let ml_external_type fmt (`Prim (args, ret) : ml_external_type) =
    List.iter (fprintf fmt "@[%a@ ->@]@ " (ml_type ArrowParens)) args;
    ml_type ArrowParens fmt ret

  let primname_opt fmt = function
    | None -> ()
    | Some primname -> fprintf fmt "%S" primname

  let attrs fmt { float; noalloc } =
    begin 
    (* TODO: float support not yet implemented *)
    (* if float then pp_print_string fmt "\"float\""; *)

    (* TODO: fix this.  The may_allocate function determines whether any of
       the functions in the generated C cause OCaml heap allocations.
       However, it doesn't currently account for callbacks: if we pass a
       handle to an OCaml function into C, calling the function can trigger an
       allocation.  We need some way in the interface of the library for the
       client to indicate whether it is safe to assume that a C function
       cannot call back into OCaml. *)
    (* if noalloc then pp_print_string fmt "\"noalloc\"" *)
    end

  let rec ml_exp appl_parens fmt (e : ml_exp) =
    match appl_parens, e with
    | _, `Ident x -> ident fmt x
    | _, `Project (e, l) -> fprintf fmt "%a.%a" (ml_exp ApplParens) e ident l
    | ApplParens, `Appl (f, p) -> fprintf fmt "@[(%a@;<1 2>%a)@]" (ml_exp NoApplParens) f (ml_exp ApplParens) p
    | NoApplParens, `Appl (f, p) -> fprintf fmt "@[%a@ %a@]" (ml_exp NoApplParens) f (ml_exp ApplParens) p
    | ApplParens, `MakePtr (Ty t, e) ->
      fprintf fmt
        "(@[<hov 2>Cstubs_internals.make_ptr@ %a@ %a)@]" (typ ApplParens) t (ml_exp ApplParens) e
    | NoApplParens, `MakePtr (Ty t, e) ->
      fprintf fmt
        "@[<hov 2>Cstubs_internals.make_ptr@ %a@ %a@]" (typ ApplParens) t (ml_exp ApplParens) e
    | ApplParens, `MakeStructured (Ty t, e) ->
      fprintf fmt
        "(@[<hov 2>Cstubs_internals.make_structured@ %a@ %a)@]" (typ ApplParens) t (ml_exp ApplParens) e
    | NoApplParens, `MakeStructured (Ty t, e) ->
      fprintf fmt
        "@[<hov 2>Cstubs_internals.make_structured@ %a@ %a@]" (typ ApplParens) t (ml_exp ApplParens) e
    | ApplParens, `Coerce (Ty t1, Ty t2, e) ->
      fprintf fmt "@[(Ctypes.coerce@;<1 2>%a@ %a@ %a)@]"
        (typ ApplParens) t1 (typ ApplParens) t2 (ml_exp ApplParens) e
    | NoApplParens, `Coerce (Ty t1, Ty t2, e) ->
      fprintf fmt "@[Ctypes.coerce@;<1 2>%a@ %a@ %a@]"
        (typ ApplParens) t1 (typ ApplParens) t2 (ml_exp ApplParens) e

  let args fmt xs =
    List.iter (fprintf fmt "%s@ ") xs

  let ml_dec fmt (`Function (f, qt, xs, e) : ml_dec) =
    fprintf fmt
      "@[<v>@[let@ %s@ :@]@[<hov 1>@ %a@ @]@;<1 2>@[<hov 4>=@ fun@ %a->@ %a@]@\n@]@."
      f (ml_type NoArrowParens) qt args xs (ml_exp NoApplParens) e

  let extern fmt { ident; typ; primname; primname_byte; attributes } =
    fprintf fmt
      "@[<v>@[external@ %s@ :@]@[<hov 1>@ %a@ @]@;<1 2>"
      ident ml_external_type typ;
    fprintf fmt
      "@[=@[@[%a@]@ @[%S@]@ %a@]@]@]@."
      primname_opt primname_byte primname attrs attributes
end

let rec arity : ml_external_type -> int =
  fun (`Prim (args, _)) -> List.length args

let max_byte_args = 5

let byte_stub_name : string -> ml_external_type -> string option =
  fun name t ->
    let arity = arity t in
    if arity > max_byte_args
    then Some (Printf.sprintf "%s_byte%d" name arity)
    else None

let attributes : type a b. (a -> b) fn -> attributes =
   let open Cstubs_analysis in
   fun fn -> { float = float fn; noalloc = not (may_allocate fn) }

let managed_buffer = `Ident (path_of_string "Memory_stubs.managed_buffer")
let voidp = `Ident (path_of_string "Ctypes_raw.voidp")

(* These functions determine the type that should appear in the extern
   signature *)
let rec ml_typ_of_return_typ : type a. a typ -> ml_type =
  function
  | Void -> `Ident (path_of_string "unit")
  | Primitive p -> `Ident (Cstubs_public_name.ident_of_ml_prim (Primitives.ml_prim p))
  | Struct _    -> managed_buffer
  | Union _     -> managed_buffer
  | Abstract _  -> managed_buffer
  | Pointer _   -> voidp
  | View { ty } -> ml_typ_of_return_typ ty
  | Array _    as a -> internal_error
    "Unexpected array type in the return type: %s" (Ctypes.string_of_typ a)
  | Bigarray _ as a -> internal_error
    "Unexpected bigarray type in the return type: %s" (Ctypes.string_of_typ a)

let rec ml_typ_of_arg_typ : type a. a typ -> ml_type = function
  | Void -> `Ident (path_of_string "unit")
  | Primitive p -> `Ident (Cstubs_public_name.ident_of_ml_prim (Primitives.ml_prim p))
  | Pointer _   -> voidp
  | Struct _    -> voidp
  | Union _     -> voidp
  | Abstract _  -> voidp
  | View { ty } -> ml_typ_of_arg_typ ty
  | Array _    as a -> internal_error
    "Unexpected array in an argument type: %s" (Ctypes.string_of_typ a)
  | Bigarray _ as a -> internal_error
    "Unexpected bigarray in an argument type: %s" (Ctypes.string_of_typ a)

let rec ml_external_type_of_fn : type a. a fn -> ml_external_type = function
  | Returns t -> `Prim ([], ml_typ_of_return_typ t)
  | Function (f, t) ->
    let `Prim (l, t) = ml_external_type_of_fn t in
    `Prim (ml_typ_of_arg_typ f :: l, t)

let rec underlying : type a. a typ -> ty = function
  | View { ty } -> underlying ty
  | t -> Ty t

let rec unwrap_val : type a. a typ -> ml_exp -> ml_exp option =
  fun ty e -> match ty with
  | Void -> None
  | Primitive p -> None
  | Pointer _ -> Some (`Project (e, path_of_string "Cstubs_internals.raw_ptr"))
  | Struct _ -> Some (`Project (`Appl (`Ident (path_of_string "Ctypes.addr"), e),
                                path_of_string "Cstubs_internals.raw_ptr"))
  | Union _ -> Some (`Project (`Appl (`Ident (path_of_string "Ctypes.addr"), e),
                               path_of_string "Cstubs_internals.raw_ptr"))
  | View _  ->
    let Ty u = underlying ty in
    let e = `Coerce (Ty ty, Ty u, e) in
    begin match unwrap_val u e with
    | Some e -> Some e
    | None -> Some e
    end
  | Abstract _ -> internal_error
    "Unexpected abstract type encountered during ML code generation: %s"
    (Ctypes.string_of_typ ty)
  | Array _ -> internal_error
    "Unexpected array type encountered during ML code generation: %s"
    (Ctypes.string_of_typ ty)
  | Bigarray _ -> internal_error
    "Unexpected bigarray type encountered during ML code generation: %s"
    (Ctypes.string_of_typ ty)

let rec wrap_val : type a. a typ -> ml_exp -> ml_exp option =
  fun ty x -> match ty with
  | Void -> None
  | Primitive p -> None
  | Pointer t -> Some (`MakePtr (Ty t, x))
  | Struct _ -> Some (`MakeStructured (Ty ty, x))
  | Union _ -> Some (`MakeStructured (Ty ty, x))
  | View _ ->
    let Ty u = underlying ty in
    begin match wrap_val u x with
    | Some x ->
      Some (`Coerce (Ty u, Ty ty, x))
    | None ->
      Some (`Coerce (Ty u, Ty ty, x))
    end
  | Abstract _ -> internal_error
    "Unexpected abstract type encountered during ML code generation: %s"
    (Ctypes.string_of_typ ty)
  | Array _ -> internal_error
    "Unexpected array type encountered during ML code generation: %s"
    (Ctypes.string_of_typ ty)
  | Bigarray _ -> internal_error
    "Unexpected bigarray type encountered during ML code generation: %s"
    (Ctypes.string_of_typ ty)

let var_counter = ref 0
let fresh_var () =
  incr var_counter;
  Printf.sprintf "x%d" !var_counter

type wrapper_state = {
  exp: ml_exp;
  args: lident list;
  trivial: bool;
}
let rec wrapper_body : type a. a fn -> ml_exp -> wrapper_state =
  fun fn exp -> match fn with
  | Returns t ->
    begin match wrap_val t exp with
      None -> { exp ; args = []; trivial = true }
    | Some exp -> { exp; args = []; trivial = false }
    end
  | Function (f, t) ->
    let x = fresh_var () in
    begin match unwrap_val f (`Ident (path_of_string x)) with
    | None ->
      let { exp; args; trivial } = wrapper_body t (`Appl (exp, `Ident (path_of_string x))) in
      { exp; args = x :: args; trivial }
    | Some exp' ->
      let { exp; args = xs; trivial } = wrapper_body t (`Appl (exp, exp')) in
      { exp; args = x :: xs; trivial = false }
    end

let wrapper : type a. a fn -> string -> ml_dec option =
  fun fn f ->
    match wrapper_body fn (`Ident (path_of_string f)) with
    | { trivial = true } ->
      None
    | { exp; args } ->
      Some (`Function (f, Cstubs_public_name.ident_of_fn fn, args, exp))

let fn : type a b. stub_name:string -> external_name:string -> (a -> b) fn -> extern * ml_dec option =
   fun ~stub_name ~external_name fn -> 
     let typ = ml_external_type_of_fn fn in
     ({ ident = external_name;
        typ = typ;
        primname = stub_name;
        primname_byte = byte_stub_name stub_name typ;
        attributes = attributes fn; },
      wrapper fn external_name)

let fn ~stub_name ~external_name fmt f =
  match fn ~stub_name ~external_name f with
    (ext, Some dec) ->
      Emit_ML.extern fmt ext;
      Emit_ML.ml_dec fmt dec
  | (ext, None) ->
      Emit_ML.extern fmt ext;
      Format.fprintf fmt "@."

let signature s fmt f =
  Format.fprintf fmt "val@ %s@ :@ %a@."
    s Emit_ML.(ml_type NoArrowParens) (Cstubs_public_name.ident_of_fn f)
