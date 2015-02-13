(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Ctypes

module type TYPE =
sig
  include Ctypes_types.TYPE

  type 'a const
  val constant : string -> 'a typ -> 'a const

  val enum : string -> ?unexpected:(int64 -> 'a) -> ('a * int64 const) list -> 'a typ
end

module type BINDINGS = functor (F : TYPE) -> sig end

let cstring s =
  (* Format a string for output as a C string literal. *)
  let mappings = [Str.regexp "\"", "\\\"";
                  Str.regexp "\n", "\\n"] in
  let escaped =
    List.fold_left (fun s (r, r') -> Str.(global_replace r r') s) s mappings
  in "\""^ escaped ^"\""

let cprologue = [
  "#if !__USE_MINGW_ANSI_STDIO && (defined(__MINGW32__) || defined(__MINGW64__))";
  "#define __USE_MINGW_ANSI_STDIO 1";
  "#endif";
  "";
  "#include <stdio.h>";
  "#include <stddef.h>";
  "#include \"ctypes_cstubs_internals.h\"";
  "";
  "int main(void)";
  "{";
  ]
let cepilogue = [
  "  return 0;";
  "}";
  ]
let mlprologue = [
  "include Ctypes";
  "let lift x = x";
  "open Ctypes_static";
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
  | `Struct tag, name ->
    let foffset fmt = offsetof ("struct " ^ tag) name fmt in
    puts (Printf.sprintf "  | Struct ({ tag = %S} as s'), %S ->" tag name) fmt;
    printf1              "    let f = {ftype; fname; foffset = %zu} in \n" foffset fmt;
    puts                 "    (s'.fields <- BoxedField f :: s'.fields; f)" fmt;
  | `Union tag, name ->
    let foffset fmt = offsetof ("union " ^ tag) name fmt in
    puts (Printf.sprintf "  | Union ({ utag = %S} as s'), %S ->" tag name) fmt;
    printf1              "    let f = {ftype; fname; foffset = %zu} in \n" foffset fmt;
    puts                 "    (s'.ufields <- BoxedField f :: s'.ufields; f)" fmt;
  | _ -> raise (Unsupported "Adding a field to non-structured type")
  in
  cases fmt specs
  ["";
   "let field : type a s. 't typ -> string -> a typ ->";
   "  (a, ((s, [<`Struct | `Union]) structured as 't)) field =";
   "  fun (type k) (s : (_, k) structured typ) fname ftype -> match s, fname with";]
  ~case
  ["  | _ -> failwith (\"Unexpected field \"^ fname)"]

let write_seal fmt specs =
  let case = function
    | `Struct tag ->
        let ssize fmt = sizeof ~fmt ("struct " ^ tag)
        and salign fmt = offsetof ~fmt ("struct { char c; struct "^ tag ^" x; }") "x" in
        puts ~fmt (Printf.sprintf "  | Struct ({ tag = %S; spec = Incomplete _ } as s') ->" tag);
        printf2 ~fmt              "    s'.spec <- Complete { size = %zu; align = %zu }\n" ssize salign;
    | `Union tag ->
        let usize fmt = sizeof ~fmt ("union " ^ tag)
        and ualign fmt = offsetof ~fmt ("struct { char c; union "^ tag ^" x; }") "x" in
        puts ~fmt (Printf.sprintf "  | Union ({ utag = %S; uspec = None } as s') ->" tag);
        printf2 ~fmt              "    s'.uspec <- Some { size = %zu; align = %zu }\n" usize ualign;
    | `Other -> 
      raise (Unsupported "Sealing a non-structured type")
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

let primitive_format_string : type a. a Ctypes_primitive_types.prim -> string =
  fun p ->
    let open Ctypes_primitive_types in
    let sprintf = Printf.sprintf in
    let fail () =
      Printf.kprintf failwith "Cannot retrieve constants of type %s"
        (Ctypes_primitives.name p)
    in
    match p, Ctypes_primitives.format_string p with
    | _, None -> fail ()
    | Char, Some fmt -> sprintf "Char.chr (((%s) + 256) mod 256)" fmt
    | Schar, Some fmt -> fmt
    | Uchar, Some fmt -> sprintf "Unsigned.UChar.of_string \"%s\"" fmt
    | Bool, Some fmt -> sprintf "((%s) <> 0)" fmt
    | Short, Some fmt -> fmt
    | Int, Some fmt -> fmt
    | Long, Some fmt -> sprintf "Signed.Long.of_string \"%s\"" fmt
    | Llong, Some fmt -> sprintf "Signed.LLong.of_string \"%s\"" fmt
    | Ushort, Some fmt -> sprintf "Unsigned.UShort.of_string \"%s\"" fmt
    | Uint, Some fmt -> sprintf "Unsigned.UInt.of_string \"%s\"" fmt
    | Ulong, Some fmt -> sprintf "Unsigned.ULong.of_string \"%s\"" fmt
    | Ullong, Some fmt -> sprintf "Unsigned.ULLong.of_string \"%s\"" fmt
    | Size_t, Some fmt -> sprintf "Unsigned.Size_t.of_string \"%s\"" fmt
    | Int8_t, Some fmt -> fmt
    | Int16_t, Some fmt -> fmt
    | Int32_t, Some fmt -> fmt ^"l"
    | Int64_t, Some fmt -> fmt ^"L"
    | Uint8_t, Some fmt -> sprintf "Unsigned.UInt8.of_string \"%s\"" fmt
    | Uint16_t, Some fmt -> sprintf "Unsigned.UInt16.of_string \"%s\"" fmt
    | Uint32_t, Some fmt -> sprintf "Unsigned.UInt32.of_string \"%s\"" fmt
    | Uint64_t, Some fmt -> sprintf "Unsigned.UInt64.of_string \"%s\"" fmt
    | Camlint, Some fmt -> fmt
    | Nativeint, Some fmt -> fmt ^"n"
    (* Integer constant expressions cannot have non-integer type *)
    | Complex32, _ -> fail ()
    | Complex64, _ -> fail ()
    | Float, _ -> fail ()
    | Double, _ -> fail ()

let rec ml_pat_and_exp_of_typ : type a. a typ -> string * string =
  fun ty -> 
    match ty with
    | Ctypes_static.View { Ctypes_static.ty } ->
      let p, e = ml_pat_and_exp_of_typ ty in
      let x = Cstubs_c_language.fresh_var ~prefix:"read" () in
      let p' = Printf.sprintf "Ctypes_static.View { Ctypes_static.read = %s; ty = %s }" x p
      and e' = Printf.sprintf "(%s (%s))" x e in
      (p', e')
    | Ctypes_static.Primitive p ->
      let pat = 
        (Ctypes_common.asprintf "Ctypes_static.Primitive %a"
           Ctypes_path.format_path
           (Cstubs_public_name.constructor_cident_of_prim p))
      and exp = primitive_format_string p in
      (pat, exp)
    | _ -> failwith "constant of non-primitive"

let write_consts fmt consts =
  let case =
    function (name, Ctypes_static.BoxedType ty) ->
      let p, e = ml_pat_and_exp_of_typ ty in
      Format.fprintf fmt "{@[<v 2>@\n";
      Format.fprintf fmt "enum { check_%s_const = (int)%s };@\n" name name;
      (* Since printf is variadic we can't rely on implicit conversions.
         We'll use assignment rather than casts to coerce to the correct type
         because casts typically result in silent truncation whereas narrowing
         assignments typically trigger warnings even on default compiler
         settings. *)
      Format.fprintf fmt "%a = (%s);@\n" (Ctypes.format_typ ~name:"v") ty name;
      printf1 ~fmt
        (Ctypes_common.asprintf "  | %S, %s ->@\n    %s\n" name p e)
        (fun fmt -> Format.fprintf fmt "v");
      Format.fprintf fmt "@]@\n}@\n"
  in
  cases fmt consts
    ["type 'a const = 'a";
     "let constant (type t) name (t : t typ) : t = match name, t with"]
    ~case
    ["  | s, _ -> failwith (\"unmatched constant: \"^ s)"] 
    

let write_enums fmt enums =
  let case name =
    printf1 ~fmt
      (Format.sprintf
         "  | %S -> \n    Cstubs_internals.build_enum_type %S Ctypes_static.%%s ?unexpected alist\n"
         name
         name)
      (fun fmt ->
         Format.fprintf fmt
           "ctypes_arithmetic_type_name(CTYPES_CLASSIFY_ARITHMETIC_TYPE(enum %s))"
           name)
  in
  cases fmt enums
    ["";
     "let enum (type a) name ?unexpected (alist : (a * int64) list) =";
     "  match name with"]
    ~case
    ["  | s ->";
     "    failwith (\"unmatched enum: \"^ s)"]


let write_ml fmt fields structures consts enums =
  List.iter (puts ~fmt) mlprologue;
  write_field fmt fields;
  write_seal fmt structures;
  write_consts fmt consts;
  write_enums fmt enums

let gen_c () =
  let fields = ref []
  and structures = ref []
  and consts = ref []
  and enums = ref []
  in
  let finally fmt = write_c fmt (fun fmt ->
                    write_ml fmt !fields !structures !consts !enums) in
  let m =
    (module struct
      include Ctypes
      let field (type s) (s : (_, s) structured typ) fname ftype =
        let () = match s with 
        | Ctypes_static.Struct { Ctypes_static.tag } ->
          fields := (`Struct tag, fname) :: !fields
        | Ctypes_static.Union { Ctypes_static.utag } ->
          fields := (`Union utag, fname) :: !fields
        | _ ->
          ()
        in { Ctypes_static.ftype; foffset = -1; fname}
      let seal (type s) (s : (_, s) structured typ) =
        match s with
        | Ctypes_static.Struct { Ctypes_static.tag } ->
          structures := `Struct tag :: !structures
        | Ctypes_static.Union { Ctypes_static.utag } ->
          structures := `Union utag :: !structures
        | _ ->
          ()
      type _ const = unit
      let constant name ty  = consts := (name, Ctypes_static.BoxedType ty) :: !consts
      let enum name ?unexpected alist =
        let () = enums := name :: !enums in
        let format_typ k fmt = Format.fprintf fmt "enum %s%t" name k in
        (* a dummy value of type 'a typ, mostly unusable *)
        view void
          ~format_typ
          ~read:(fun _ -> assert false)
          ~write:(fun _ -> assert false)
     end : TYPE)
  in (m, finally)

let write_c fmt (module B : BINDINGS) =
  let m, finally = gen_c () in
  let module M = B((val m)) in
  finally fmt
