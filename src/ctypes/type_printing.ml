(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Static

(* The format context affects the formatting of pointer, struct and union
   types.  There are three printing contexts: *)
type format_context = [
(* In the top-level context struct and union types are printed in full, with
   member lists.  Pointer types are unparenthesized; for example,
   pointer-to-void is printed as "void *", not as "void ( * )". *)
| `toplevel
(* In the array context, struct and union types are printed in abbreviated
   form, which consists of just a keyword and the tag name.  Pointer types are
   parenthesized; for example, pointer-to-array-of-int is printed as
   "int ( * )[]", not as "int *[]". *)
| `array
(* In the non-array context, struct and union types are printed in abbreviated
   form and pointer types are unparenthesized. *)
| `nonarray]

let rec format_typ : type a. a typ ->
  (format_context -> Format.formatter -> unit) ->
  (format_context -> Format.formatter -> unit) =
  let fprintf = Format.fprintf in 
  fun t k context fmt -> match t with
    | Void ->
      fprintf fmt "void%t" (k `nonarray)
    | Primitive p ->
      let name = Ctypes_primitives.name p in
      fprintf fmt "%s%t" name (k `nonarray)
    | View { format_typ = Some format } ->
      format (k `nonarray) fmt
    | View { ty } ->
      format_typ ty k context fmt
    | Abstract { aname } ->
      fprintf fmt "%s%t" aname (k `nonarray)
    | Struct { tag ; spec; fields } ->
      begin match spec, context with
        | Complete _, `toplevel ->
          begin
            fprintf fmt "struct %s {@;<1 2>@[" tag;
            format_fields fields fmt;
            fprintf fmt "@]@;}%t" (k `nonarray)
          end
        | _ -> fprintf fmt "struct %s%t" tag (k `nonarray)
      end
    | Union { utag; uspec; ufields } ->
      begin match uspec, context with
        | Some _, `toplevel ->
          begin
            fprintf fmt "union %s {@;<1 2>@[" utag;
            format_fields ufields fmt;
            fprintf fmt "@]@;}%t" (k `nonarray)
          end
        | _ -> fprintf fmt "union %s%t" utag (k `nonarray)
      end
    | Pointer ty ->
      format_typ ty
        (fun context fmt ->
          match context with
            | `array -> fprintf fmt "(*%t)" (k `nonarray)
            | _      -> fprintf fmt "*%t" (k `nonarray))
        `nonarray fmt
    | Array (ty, n) ->
      format_typ ty (fun _ fmt -> fprintf fmt "%t[%d]" (k `array) n) `nonarray
        fmt
    | Bigarray ba ->
      let elem = Ctypes_bigarray.element_type ba
      and dims = Ctypes_bigarray.dimensions ba in
      let name = Ctypes_primitives.name elem in
      fprintf fmt "%s%t%t" name (k `array)
        (fun fmt -> (Array.iter (Format.fprintf fmt "[%d]") dims))

and format_fields : type a. a boxed_field list -> Format.formatter -> unit =
  fun fields fmt ->
  let open Format in
      List.iteri
        (fun i (BoxedField {ftype=t; fname}) ->
          fprintf fmt "@[";
          format_typ t (fun _ fmt -> fprintf fmt " %s" fname) `nonarray fmt;
          fprintf fmt "@];@;")
        fields
and format_parameter_list parameters k fmt =
  Format.fprintf fmt "%t(@[@[" k;
  if parameters = [] then Format.fprintf fmt "void" else
    List.iteri
      (fun i (BoxedType t) ->
        if i <> 0 then Format.fprintf fmt "@], @[";
        format_typ t (fun _ _ -> ()) `nonarray fmt)
      parameters;
  Format.fprintf fmt "@]@])"
and format_fn' : 'a. 'a fn ->
  (Format.formatter -> unit) ->
  (Format.formatter -> unit) =
  let rec gather : type a. a fn -> boxed_typ list * boxed_typ =
    function
      | Returns ty -> [], BoxedType ty
      | Function (Void, fn) -> gather fn
      | Function (p, fn) -> let ps, r = gather fn in BoxedType p :: ps, r in
  fun fn k fmt ->
    let ps, BoxedType r = gather fn in
    format_typ r (fun context fmt -> format_parameter_list ps k fmt)
      `nonarray fmt

let format_name ?name fmt =
  match name with
    | Some name -> Format.fprintf fmt " %s" name
    | None      -> ()

let format_typ : ?name:string -> Format.formatter -> 'a typ -> unit
  = fun ?name fmt typ ->
    Format.fprintf fmt "@[";
    format_typ typ (fun context -> format_name ?name) `toplevel fmt;
    Format.fprintf fmt "@]"

let format_fn : ?name:string -> Format.formatter -> 'a fn -> unit
  = fun ?name fmt fn ->
    Format.fprintf fmt "@[";
    format_fn' fn (format_name ?name) fmt;
    Format.fprintf fmt "@]"

let string_of_typ ?name ty = Common.string_of (format_typ ?name) ty
let string_of_fn ?name fn = Common.string_of (format_fn ?name) fn
