(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

[@@@warning "-9-27"]

open Ctypes_static

(* See ctypes_type_printing.mli for the documentation of [format_context]. *)
type format_context = [ `toplevel | `array | `nonarray ]

let format_qualifier : Format.formatter -> qualifier -> unit =
  fun fmt q ->
  match q with
  | Const -> Format.fprintf fmt "const"
  | Volatile -> Format.fprintf fmt "volatile"

let rec format_typ' : type a. a typ ->
  (format_context -> Format.formatter -> unit) ->
  (format_context -> Format.formatter -> unit) =
  let fprintf = Format.fprintf in
  fun t k ctxt fmt -> match t, ctxt with
    | Void, _ ->
      fprintf fmt "void%t" (k `nonarray)
    | Primitive p, _ ->
      let name = Ctypes_primitives.name p in
      fprintf fmt "%s%t" name (k `nonarray)
    | View { format_typ = Some format }, _ ->
      format (k `nonarray) fmt
    | View { ty }, context ->
      format_typ' ty k context fmt
    | Qualified (q, ty), ctxt ->
       format_typ' ty 
         (fun context fmt ->
           fprintf fmt "@ %a%t" format_qualifier q (k context)) ctxt fmt
    | Abstract { aname }, _ ->
      fprintf fmt "%s%t" aname (k `nonarray)
    | Struct { tag = "" ; fields }, _ ->
      fprintf fmt "struct {@;<1 2>@[";
      format_fields fields fmt;
      fprintf fmt "@]@;}%t" (k `nonarray)
    | Struct { tag ; spec = Complete _; fields }, `toplevel ->
       fprintf fmt "struct %s {@;<1 2>@[" tag;
       format_fields fields fmt;
       fprintf fmt "@]@;}%t" (k `nonarray)
    | Struct { tag ; _ }, _ ->
       fprintf fmt "struct %s%t" tag (k `nonarray)
    | Union { utag = ""; ufields }, _ ->
      fprintf fmt "union {@;<1 2>@[";
      format_fields ufields fmt;
      fprintf fmt "@]@;}%t" (k `nonarray)
    | Union { utag; uspec = Some _; ufields }, `toplevel ->
            fprintf fmt "union %s {@;<1 2>@[" utag;
            format_fields ufields fmt;
            fprintf fmt "@]@;}%t" (k `nonarray)
    | Union { utag; _ }, context ->
       fprintf fmt "union %s%t" utag (k `nonarray)
    | Pointer ty, _ ->
      format_typ' ty
        (fun context fmt ->
          match context with
            | `array -> fprintf fmt "(*%t)" (k `nonarray)
            | _      -> fprintf fmt "*%t" (k `nonarray))
        `nonarray fmt
    | Funptr fn, _ ->
      format_fn' fn
        (fun fmt -> Format.fprintf fmt "(*%t)" (k `nonarray)) fmt
    | Array (ty, n), _ ->
      format_typ' ty (fun _ fmt -> fprintf fmt "%t[%d]" (k `array) n) `nonarray
        fmt
    | Bigarray ba, _ ->
      let elem = Ctypes_bigarray.element_type ba
      and dims = Ctypes_bigarray.dimensions ba in
      let name = Ctypes_primitives.name elem in
      fprintf fmt "%s%t%t" name (k `array)
        (fun fmt -> (Array.iter (Format.fprintf fmt "[%d]") dims))
    | OCaml String, context -> format_typ' (ptr char) k context fmt
    | OCaml Bytes, context -> format_typ' (ptr uchar) k context fmt
    | OCaml FloatArray, context -> format_typ' (ptr double) k context fmt

and format_fields : type a. a boxed_field list -> Format.formatter -> unit =
  fun fields fmt ->
  let open Format in
      List.iteri
        (fun i (BoxedField {ftype=t; fname}) ->
          fprintf fmt "@[";
          format_typ' t (fun _ fmt -> fprintf fmt " %s" fname) `nonarray fmt;
          fprintf fmt "@];@;")
        fields
and format_parameter_list parameters k fmt =
  Format.fprintf fmt "%t(@[@[" k;
  if parameters = [] then Format.fprintf fmt "void" else
    List.iteri
      (fun i (BoxedType t) ->
        if i <> 0 then Format.fprintf fmt "@], @[";
        format_typ' t (fun _ _ -> ()) `nonarray fmt)
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
    format_typ' r (fun context fmt -> format_parameter_list ps k fmt)
      `nonarray fmt

let format_name ?name fmt =
  match name with
    | Some name -> Format.fprintf fmt " %s" name
    | None      -> ()

let format_typ : ?name:string -> Format.formatter -> 'a typ -> unit
  = fun ?name fmt typ ->
    Format.fprintf fmt "@[";
    format_typ' typ (fun context -> format_name ?name) `toplevel fmt;
    Format.fprintf fmt "@]"

let format_fn : ?name:string -> Format.formatter -> 'a fn -> unit
  = fun ?name fmt fn ->
    Format.fprintf fmt "@[";
    format_fn' fn (format_name ?name) fmt;
    Format.fprintf fmt "@]"

let string_of_typ ?name ty = Format.asprintf "%a" (format_typ ?name) ty
let string_of_fn ?name fn = Format.asprintf "%a" (format_fn ?name) fn
