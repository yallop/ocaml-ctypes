(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Ctypes

module type STRUCT =
sig
  type _ typ
  type (_, _) field

  val structure : string -> 's structure typ
  val union : string -> 's union typ

  val field : 't typ -> string -> 'a Ctypes.typ ->
    ('a, (('s, [<`Struct | `Union]) structured as 't)) field

  val seal : (_, [< `Struct | `Union]) structured typ -> unit
end

module type BINDINGS = functor (F : STRUCT) -> sig end

let cstring s =
  (* Format a string for output as a C string literal. *)
  let mappings = [Str.regexp "\"", "\\\"";
                  Str.regexp "\n", "\\n"] in
  let escaped =
    List.fold_left (fun s (r, r') -> Str.(global_replace r r') s) s mappings
  in "\""^ escaped ^"\""

let cprologue = [
  "#include <stdio.h>";
  "#include <stddef.h>";
  "";
  "int main(void)";
  "{";
  ]
let cepilogue = [
  "  return 0;";
  "}";
  ]
let mlprologue = [
  "open Static";

  "let (structure, union) = Static.(structure, union)";
  "";
  "type 'a typ = 'a Static.typ";
  "type ('a, 's) field = ('a, 's) Static.field";
]

(* [puts fmt s] writes the call [puts(s);] on [fmt]. *)
let puts s ~fmt = Format.fprintf fmt "@[puts(@[%s);@]@]@\n"
  (cstring s)

(* [printf1 s v fmt] writes the call [printf(s, v);] on [fmt]. *)
let printf1 s v ~fmt = Format.fprintf fmt "@[printf(@[%s,@ %t);@]@]@\n"
  (cstring s) v

(* [printf2 s u v fmt] writes the call [printf(s, u, v);] on [fmt]. *)
let printf2 s u v ~fmt = Format.fprintf fmt "@[printf(@[%s,@ %t,@ %t);@]@]@\n"
  (cstring s) u v

(* [offsetof t f fmt] writes the call [offsetof(t, f)] on [fmt]. *) 
let offsetof t f ~fmt = Format.fprintf fmt "@[offsetof(@[%s,@ %s)@]@]" t f

(* [sizeof t fmt] writes the call [sizeof(t)] on [fmt]. *) 
let sizeof t ~fmt = Format.fprintf fmt "@[sizeof(@[%s)@]@]" t

let write_c fmt body =
  List.iter (Format.fprintf fmt "@[%s@]@\n") cprologue;
  Format.fprintf fmt "@[<v 2>@\n@[%t@]@]@\n" body;
  List.iter (Format.fprintf fmt "%s@\n") cepilogue

let cases fmt list prologue epilogue ~case =
  List.iter (puts ~fmt) prologue;
  List.iter case list;
  List.iter (puts ~fmt) epilogue

let write_field fmt specs =
  let case = function
  | `Struct, tag, name ->
    let foffset fmt = offsetof ("struct " ^ tag) name fmt in
    puts (Printf.sprintf "  | Struct ({ tag = %S} as s'), %S ->" tag name) fmt;
    printf1              "    let f = {ftype; fname; foffset = %zu} in \n" foffset fmt;
    puts                 "    (s'.fields <- BoxedField f :: s'.fields; f)" fmt;
  | `Union, tag, name ->
    let foffset fmt = offsetof ("union " ^ tag) name fmt in
    puts (Printf.sprintf "  | Union ({ utag = %S} as s'), %S ->" tag name) fmt;
    printf1              "    let f = {ftype; fname; foffset = %zu} in \n" foffset fmt;
    puts                 "    (s'.ufields <- BoxedField f :: s'.ufields; f)" fmt;
  in
  cases fmt specs
  ["";
   "let field : type a s. 't typ -> string -> a typ ->";
   "  (a, ((s, [<`Struct | `Union]) structured as 't)) field =";
   "  fun (type k) (s : (_, k) structured typ) fname ftype -> match s, fname with";]
  ~case
  ["  | _ -> failwith (\"Unexpected field \"^ fname)"]

let write_seal fmt specs =
  let case (kw, tag) = match kw with
    | `Struct ->
        let ssize fmt = sizeof ~fmt ("struct " ^ tag)
        and salign fmt = offsetof ~fmt ("struct { char c; struct "^ tag ^" x; }") "x" in
        puts ~fmt (Printf.sprintf "  | Struct ({ tag = %S; spec = Incomplete _ } as s') ->" tag);
        printf2 ~fmt              "    s'.spec <- Complete { size = %zu; align = %zu }\n" ssize salign;
    | `Union ->
        let usize fmt = sizeof ~fmt ("union " ^ tag)
        and ualign fmt = offsetof ~fmt ("struct { char c; union "^ tag ^" x; }") "x" in
        puts ~fmt (Printf.sprintf "  | Union ({ utag = %S; uspec = None } as s') ->" tag);
        printf2 ~fmt              "    s'.uspec <- Some { size = %zu; align = %zu }\n" usize ualign;
  in
  cases fmt specs
    ["";
     "let seal (type t) (t : (_, t) structured typ) = match t with"]
    ~case
    ["  | Struct { tag; spec = Complete _; } ->";
     "    raise (ModifyingSealedType tag)";
     "  | Union { utag; uspec = Some _; } ->";
     "    raise (ModifyingSealedType utag)";
     "  | _ ->";
     "    raise (Unsupported \"Sealing a non-structured type\")"]

let write_ml fmt fields structures =
  List.iter (puts ~fmt) mlprologue;
  write_field fmt fields;
  write_seal fmt structures

let gen_c () =
  let fields = ref [] and structures = ref [] in
  let finally fmt = write_c fmt (fun fmt -> write_ml fmt !fields !structures) in
  let m = 
    (module struct
      type _ typ = [`Struct | `Union] * string
      type (_, _) field = unit
      let structure tag = (`Struct, tag)
      let union tag = (`Union, tag)
      let field (tag, kw) name _ = fields := (tag, kw, name) :: !fields
      let seal (structure : _ typ) = structures := structure :: !structures
     end : STRUCT)
  in (m, finally)

let write_c fmt (module B : BINDINGS) =
  let m, finally = gen_c () in
  let module M = B((val m)) in
  finally fmt
