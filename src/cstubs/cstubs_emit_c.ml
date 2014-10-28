(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* C pretty printing. *)

open Static
open Cstubs_c_language
open Format

let format_ty fmt ty = Ctypes.format_typ fmt ty

let cvar_name = function
  | `Local { name } | `Global { name } -> name

let cvar fmt v = fprintf fmt "%s" (cvar_name v)

let cconst : type a. formatter -> a cconst -> unit = fun fmt -> function
  | CInt i -> fprintf fmt "%d" i
  | CSizeof t -> fprintf fmt "sizeof (%a)" format_typ t

(* Determine whether the C expression [(ty)e] is equivalent to [e] *)
type prim = Prim : _ Primitives.prim -> prim
let cast_unnecessary : type a b. a typ -> b cexp -> bool =
  let rec harmless : type a b. a Static.typ -> b Static.typ -> bool =
    fun l r -> match l, r with
  | Pointer Void, Pointer _ -> true
  | View { Static.ty }, t -> harmless ty t
  | t,  View { Static.ty } -> harmless t ty
  | Primitive l, Primitive r -> Prim l = Prim r
  | _ -> false
  in
  (* Here *)
  fun {ty_} e -> harmless ty_ (Type_C.cexp e).ty_

let rec cexp : type a. formatter -> a cexp -> unit = fun fmt -> function
  | CConst c  -> cconst fmt c
  | CLocal x -> cvar fmt (`Local x)
  | CCast (ty, e) when cast_unnecessary ty e -> cexp fmt e
  | CCast (ty, e) -> fprintf fmt "@[@[(%a)@]%a@]" Cstubs_c_language.format_typ ty cexp e
  | CAddr e -> fprintf fmt "@[&@[%a@]@]" cexp e

let rec clvalue : type a. formatter -> a clvalue -> unit = fun fmt -> function
  | CVar x -> cvar fmt x
  | CAIndex_ (lv, i) ->
    fprintf fmt "@[@[%a@]@[[%a]@]@]" clvalue lv cexp i
  | CPIndex_ (lv, i) ->
    fprintf fmt "@[@[%a@]@[[%a]@]@]" clvalue lv cexp i

let camlop fmt : camlop -> unit = function
  | `CAMLparam0 -> fprintf fmt "CAMLparam0()"
  | `CAMLlocalN ({name}, c) -> fprintf fmt "CAMLlocalN(@[%s@],@ @[%a@])"
    name cexp c

let rec ceff : type a. formatter -> a ceff -> unit = fun fmt -> function
  | CExp e -> cexp fmt e
  | CamlOp o -> camlop fmt o
  | CGlobal x -> cvar fmt (`Global x)
  | CApp ({fname}, es) ->
    fprintf fmt "@[%s(@[" fname;
    let last_exp = args_length es - 1 in
    iteri_args es
     ~f:{ argf = fun i e ->
                 fprintf fmt "@[%a@]%(%)" cexp e
                         (if i <> last_exp then ",@ " else "")};
    fprintf fmt ")@]@]";
  | CIf (e, s, t) ->
    fprintf fmt "@[(%a)@]@ ?@ @[(%a)@]@ :@ @[(%a)@]" cexp e ceff s ceff t
  | CAIndex (e, i) ->
    fprintf fmt "@[@[%a@]@[[%a]@]@]" ceff e cexp i
  | CPIndex (e, i) ->
    fprintf fmt "@[@[%a@]@[[%a]@]@]" ceff e cexp i
  | CDeref e -> fprintf fmt "@[*@[%a@]@]" cexp e
  | CAssign (lv, e) ->
    fprintf fmt "@[@[%a@]@;=@;@[%a@]@]" clvalue lv ceff e

let rec ccomp : type a. formatter -> a ccomp -> unit = fun fmt -> function
  | CEff e -> fprintf fmt "@[<2>return@;@[%a@]@];" ceff e
  (* Here *)
  | CCAMLreturnT ({ty_=Void}, e) ->
    fprintf fmt "@[CAMLreturn0@];"
  | CCAMLreturn0 _ ->
    fprintf fmt "@[CAMLreturn0@];"
  | CCAMLreturnT (t, e) ->
    fprintf fmt "@[<2>CAMLreturnT(@[%a@],@;@[%a@])@];"
      format_typ t cexp e
  | CLet (CBind (_, (CamlOp _ as e)), s) ->
     fprintf fmt "@[%a;@]@ %a" ceff e ccomp s
  | CLet (CBind (_, (CAssign _ as e)), s) ->
     fprintf fmt "@[%a;@]@ %a" ceff e ccomp s
  | CLet (xe, CEff (CExp (CCast (ty, e')))) when cast_unnecessary ty e' ->
    ccomp fmt (CLet (xe, CEff (CExp e')))
  | CLet (CBind ({name = x}, e), CEff (CExp (CLocal {name=y}))) when x = y ->
    ccomp fmt (CEff e)
  (* Here *)
  | CLet (CBind ({typ={ty_=Void}}, CIf (e, a, b)), s) ->
    fprintf fmt "@[if@ (@[%a)@]@ {@[%a}@]@\nelse@ {@[%a}@]@]@ %a"
      cexp e ceff a ceff b ccomp s
  (* Here *)
  | CLet (CBind ({typ={ty_=Void}}, e), s) ->
    fprintf fmt "@[%a;@]@ %a" ceff e ccomp s
  (* | CLet (CBind ({name; typ={ty=Struct { tag }}}, e), s) -> *)
  (*   fprintf fmt "@[struct@;%s@;%s@;=@;@[%a;@]@]@ %a" *)
  (*     tag name ceff e ccomp s *)
  (* | CLet (CBind ({name; typ={ty=Union { utag }}}, e), s) -> *)
  (*   fprintf fmt "@[union@;%s@;%s@;=@;@[%a;@]@]@ %a" *)
  (*     utag name ceff e ccomp s *)
  | CLet (CBind ({name; typ}, e), s) ->
    fprintf fmt "@[@[%a@]@;=@;@[%a;@]@]@ %a"
      (format_decl ~name) typ ceff e ccomp s
  | CLetConst ({name=x}, c, s) ->
    fprintf fmt "@[enum@ {@[@ %s@ =@ %a@ };@]@]@ %a"
     x cconst c ccomp s

let format_params : type a. a params -> (formatter -> unit) -> (formatter -> unit) =
  fun parameters k fmt ->
  let format_param fmt {name; typ} = format_decl ~name fmt typ in
  let format_seq fmt parameters =
    fprintf fmt "(@[@[";
    iteri_params parameters
      ~f:{ paramf =
             fun i item ->
             if i <> 0 then fprintf fmt "@],@ @[";
             format_param fmt item };
    fprintf fmt "@]%s@]" ")"
  in
  match parameters with
  | NoParams -> fprintf fmt "%t(void)" k
  | _ -> fprintf fmt "@[%t@[%a@]@]" k format_seq parameters

let cfundec : type a r. formatter -> (a, r) cfundec -> unit =
  (* Here *)
  fun fmt (Fundec (name, params, {ty_=return})) ->
    Type_printing.format_typ' return
      (fun context fmt ->
       format_params params (Type_printing.format_name ~name) fmt)
      `nonarray fmt

let cfundef fmt (Fundef (dec, body) : _ cfundef) =
  fprintf fmt "%a@\n{@[<v 2>@\n%a@]@\n}@\n" 
    cfundec dec ccomp body
