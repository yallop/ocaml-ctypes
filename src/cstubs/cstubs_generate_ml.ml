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
type fnc = Fnc : 'a fn -> fnc

type ml_pat = [ `Var of string
              | `Record of (path * ml_pat) list
              | `As of ml_pat * string
              | `Underscore
              | `Con of path * ml_pat list ]

type ml_exp = [ `Ident of path 
              | `Project of ml_exp * path
              | `MakePtr of ml_exp * ml_exp
              | `MakeStructured of ml_exp * ml_exp
              | `Appl of ml_exp * ml_exp 
              | `Fun of lident list * ml_exp ]

type attributes = { float: bool; noalloc: bool }

type extern = {
  ident : string;
  typ: ml_external_type;
  primname: string;
  primname_byte: string option;
  attributes: attributes;
}

module Emit_ML : sig 
  type appl_parens = ApplParens | NoApplParens
  val ml_exp : appl_parens -> Format.formatter -> ml_exp -> unit
  val ml_pat : appl_parens -> Format.formatter -> ml_pat -> unit
  val extern : Format.formatter -> extern -> unit
end =
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

  let args fmt xs =
    List.iter (fprintf fmt "%s@ ") xs

  let rec ml_exp appl_parens fmt (e : ml_exp) =
    match appl_parens, e with
    | _, `Ident x -> ident fmt x
    | _, `Project (e, l) -> fprintf fmt "%a.%a" (ml_exp ApplParens) e ident l
    | ApplParens, `Appl (f, p) -> fprintf fmt "@[(%a@;<1 2>%a)@]" (ml_exp NoApplParens) f (ml_exp ApplParens) p
    | NoApplParens, `Appl (f, p) -> fprintf fmt "@[%a@ %a@]" (ml_exp NoApplParens) f (ml_exp ApplParens) p
    | ApplParens, `MakePtr (t, e) ->
      fprintf fmt
        "(@[<hov 2>Cstubs_internals.make_ptr@ %a@ %a)@]" (ml_exp ApplParens) t (ml_exp ApplParens) e
    | NoApplParens, `MakePtr (t, e) ->
      fprintf fmt
        "@[<hov 2>Cstubs_internals.make_ptr@ %a@ %a@]" (ml_exp ApplParens) t (ml_exp ApplParens) e
    | ApplParens, `MakeStructured (t, e) ->
      fprintf fmt
        "(@[<hov 2>Cstubs_internals.make_structured@ %a@ %a)@]" (ml_exp ApplParens) t (ml_exp ApplParens) e
    | NoApplParens, `MakeStructured (t, e) ->
      fprintf fmt
        "@[<hov 2>Cstubs_internals.make_structured@ %a@ %a@]" (ml_exp ApplParens) t (ml_exp ApplParens) e
    | _, `Fun (xs, e) ->
      fprintf fmt "(@[<1>fun@ %a->@ %a)@]" args xs (ml_exp NoApplParens) e

  let rec ml_pat appl_parens fmt pat =
    match appl_parens, pat with
    | _, `Var x -> fprintf fmt "%s" x
    | _, `Record fs -> fprintf fmt "{@[%a}@]" pat_fields fs
    | _, `As (p, x) -> fprintf fmt "@[(%a@ as@ %s)@]" (ml_pat NoApplParens) p x
    | _, `Underscore -> fprintf fmt "_"
    | _, `Con (c, []) -> fprintf fmt "%a" format_path c
    | NoApplParens, `Con (c, [p]) ->
      fprintf fmt "@[<2>%a@ @[%a@]@]" format_path c (ml_pat ApplParens) p
    | ApplParens, `Con (c, [p]) ->
      fprintf fmt "(@[<2>%a@ @[%a@])@]" format_path c (ml_pat ApplParens) p
    | ApplParens, `Con (c, ps) ->
      fprintf fmt "(@[<2>%a@ (@[%a)@])@]" format_path c pat_args ps
    | NoApplParens, `Con (c, ps) ->
      fprintf fmt "@[<2>%a@ (@[%a)@]@]" format_path c pat_args ps
  and pat_fields fmt : (path * ml_pat) list -> unit =
    List.iter
      (fun (l, p) ->
        fprintf fmt "@[%a@ =@ %a;@]@ " format_path l (ml_pat NoApplParens) p)
  and pat_args fmt : ml_pat list -> unit =
    fun xs ->
      let last = List.length xs - 1 in
      List.iteri
        (fun i ->
          if i <> last then fprintf fmt "%a,@ " (ml_pat NoApplParens)
          else fprintf fmt "%a" (ml_pat NoApplParens))
        xs

  let extern fmt { ident; typ; primname; primname_byte; attributes } =
    fprintf fmt
      "@[<v>@[external@ %s@ :@]@[<hov 1>@ %a@ @]@;<1 2>"
      ident ml_external_type typ;
    fprintf fmt
      "@[=@ @[@[%a@]@ @[%S@]@ %a@]@]@]@."
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

let attributes : type a. a fn -> attributes =
   let open Cstubs_analysis in
   fun fn -> { float = float fn; noalloc = not (may_allocate fn) }

let managed_buffer = `Ident (path_of_string "Memory_stubs.managed_buffer")
let voidp = `Ident (path_of_string "Cstubs_internals.voidp")

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

let var_counter = ref 0
let fresh_var () =
  incr var_counter;
  Printf.sprintf "x%d" !var_counter

let fn ~stub_name ~external_name fmt fn =
  let ext =
    let typ = ml_external_type_of_fn fn in
    ({ ident = external_name;
       typ = typ;
       primname = stub_name;
       primname_byte = byte_stub_name stub_name typ;
       attributes = attributes fn; }) in
  Format.fprintf fmt "%a@." Emit_ML.extern ext

let static_con c args =
  `Con (Ctypes_path.path_of_string ("Cstubs_internals." ^ c), args)

let rec pattern_and_exp_of_typ :
  type a. a typ -> ml_exp -> [`Arg | `Ret ] -> ml_pat * ml_exp option =
  fun typ e pol -> match typ with
  | Void ->
    (static_con "Void" [], None)
  | Primitive p ->
    (static_con "Primitive"
       [`Con (Cstubs_public_name.constructor_cident_of_prim p, [])],
     None)
  | Pointer _ ->
    let x = fresh_var () in
    let pat = static_con "Pointer" [`Var x] in
    begin match pol with
    | `Arg -> (pat, Some (`Project (e, path_of_string "Cstubs_internals.raw_ptr")))
    | `Ret -> (pat, Some (`MakePtr (`Ident (path_of_string x), e)))
    end
  | Struct _ ->
    let x = fresh_var () in
    let pat = `As (static_con "Struct" [`Underscore], x) in
    begin match pol with
    | `Arg ->
      (pat, Some (`Project (`Appl (`Ident (path_of_string "Ctypes.addr"), e),
                            path_of_string "Cstubs_internals.raw_ptr")))
    | `Ret -> (pat, Some (`MakeStructured (`Ident (path_of_string x), e)))
    end
  | Union _ ->
    let x = fresh_var () in
    let pat = `As (static_con "Union" [`Underscore], x) in
    begin match pol with
    | `Arg ->
      (pat, Some (`Project (`Appl (`Ident (path_of_string "Ctypes.addr"), e),
                            path_of_string "Cstubs_internals.raw_ptr")))
    | `Ret -> (pat, Some (`MakeStructured (`Ident (path_of_string x), e)))
    end
  | View { ty } ->
    begin match pol  with
    | `Arg -> 
      let x = fresh_var () in
      let e = `Appl (`Ident (path_of_string x), e) in
      let (p, None), e | (p, Some e), _ =
        pattern_and_exp_of_typ ty e pol, e in
      let pat = static_con "View"
        [`Record [path_of_string "Cstubs_internals.ty", p;
                  path_of_string "Cstubs_internals.write", `Var x]] in
      (pat, Some e)
    | `Ret -> 
      let (p, None), e | (p, Some e), _ =
        pattern_and_exp_of_typ ty e pol, e in
      let x = fresh_var () in
      let pat = static_con "View"
        [`Record [path_of_string "Cstubs_internals.ty", p;
                  path_of_string "Cstubs_internals.read", `Var x]] in
      (pat, Some (`Appl (`Ident (path_of_string x), e)))
    end
  | Abstract _ as ty -> internal_error
    "Unexpected abstract type encountered during ML code generation: %s"
    (Ctypes.string_of_typ ty)
  | Array _ as ty -> internal_error
    "Unexpected array type encountered during ML code generation: %s"
    (Ctypes.string_of_typ ty)
  | Bigarray _ as ty -> internal_error
    "Unexpected bigarray type encountered during ML code generation: %s"
    (Ctypes.string_of_typ ty)

type wrapper_state = {
  pat: ml_pat;
  exp: ml_exp;
  args: lident list;
  trivial: bool;
}

let rec wrapper_body : type a. a fn -> ml_exp -> wrapper_state =
  fun fn exp -> match fn with
  | Returns t ->
    begin match pattern_and_exp_of_typ t exp `Ret with
      pat, None -> { exp ; args = []; trivial = true;
                     pat = static_con "Returns" [pat] }
    | pat, Some exp -> { exp; args = []; trivial = false;
                         pat = static_con "Returns" [pat] }
    end
  | Function (f, t) ->
    let x = fresh_var () in
    begin match pattern_and_exp_of_typ f (`Ident (path_of_string x)) `Arg with
    | fpat, None ->
      let { exp; args; trivial; pat = tpat } =
        wrapper_body t (`Appl (exp, `Ident (path_of_string x))) in
      { exp; args = x :: args; trivial;
        pat = static_con "Function" [fpat; tpat] }
    | fpat, Some exp' ->
      let { exp; args = xs; trivial; pat = tpat } =
        wrapper_body t (`Appl (exp, exp')) in
      { exp; args = x :: xs; trivial = false;
        pat = static_con "Function" [fpat; tpat] }
    end

let wrapper : type a. a fn -> string -> ml_pat * ml_exp option =
  fun fn f -> match wrapper_body fn (`Ident (path_of_string f)) with
    { trivial = true; pat } -> (pat, None)
  | { exp; args; pat } -> (pat, Some (`Fun (args, exp)))

let case ~stub_name ~external_name fmt fn =
  let p, e = match wrapper fn external_name with
      pat, None -> pat, `Ident (path_of_string external_name)
    | pat, Some e -> pat, e
  in
  Format.fprintf fmt "@[<hov 2>@[<h 2>|@ @[%S,@ @[%a@]@]@ ->@]@ "
    stub_name Emit_ML.(ml_pat NoApplParens) p;
  Format.fprintf fmt "@[<hov 2>@[%a@]@]@]@." Emit_ML.(ml_exp ApplParens) e
