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
let puts fmt s = Format.fprintf fmt "@[puts@[(%s);@]@]@\n"
  (cstring s)

(* [printf1 fmt s v] writes the call [printf(s, v);] on [fmt]. *)
let printf1 fmt s v = Format.fprintf fmt "@[printf@[(%s,@ %t);@]@]@\n"
  (cstring s) v

(* [printf2 fmt s u v] writes the call [printf(s, u, v);] on [fmt]. *)
let printf2 fmt s u v = Format.fprintf fmt "@[printf@[(%s,@ %t,@ %t);@]@]@\n"
  (cstring s) u v

(* [offsetof fmt t f] writes the call [offsetof(t, f)] on [fmt]. *) 
let offsetof fmt (t, f) = Format.fprintf fmt "@[offsetof@[(%s,@ %s)@]@]" t f

(* [sizeof fmt t] writes the call [sizeof(t)] on [fmt]. *) 
let sizeof fmt t = Format.fprintf fmt "@[sizeof@[(%s)@]@]" t

let alignmentof fmt t =
  offsetof fmt (Format.sprintf "struct { char c; %s x; }" t, "x")

let write_c fmt body =
  List.iter (Format.fprintf fmt "@[%s@]@\n") cprologue;
  Format.fprintf fmt "@[<v 2>@\n@[%t@]@]@\n" body;
  List.iter (Format.fprintf fmt "%s@\n") cepilogue

let cases fmt list prologue epilogue ~case =
  List.iter (puts fmt) prologue;
  List.iter case list;
  List.iter (puts fmt) epilogue

let write_field fmt specs =
  let case = function
  | `Struct (tag, typedef), fname ->
    let foffset fmt = offsetof fmt (typedef, fname) in
    puts fmt (Printf.sprintf "  | Struct ({ tag = %S} as s'), %S ->" tag fname);
    printf1 fmt             "    let f = {ftype; fname; foffset = %zu} in \n" foffset;
    puts fmt                "    (s'.fields <- BoxedField f :: s'.fields; f)";
  | `Union (tag, typedef), fname ->
    let foffset fmt = offsetof fmt (typedef, fname) in
    puts fmt (Printf.sprintf "  | Union ({ utag = %S} as s'), %S ->" tag fname);
    printf1 fmt             "    let f = {ftype; fname; foffset = %zu} in \n" foffset;
    puts fmt                "    (s'.ufields <- BoxedField f :: s'.ufields; f)";
  | _ -> raise (Unsupported "Adding a field to non-structured type")
  in
  cases fmt specs
  ["";
   "let rec field : type t a. t typ -> string -> a typ -> (a, t) field =";
   "  fun s fname ftype -> match s, fname with";]
  ~case
  ["  | View { ty }, _ ->";
   "    let { ftype; foffset; fname } = field ty fname ftype in";
   "    { ftype; foffset; fname }";
   "  | _ -> failwith (\"Unexpected field \"^ fname)"]

let write_seal fmt specs =
  let case = function
    | `Struct (tag, typedef) ->
        let ssize fmt = sizeof fmt typedef
        and salign fmt = alignmentof fmt typedef in
        puts fmt (Printf.sprintf "  | Struct ({ tag = %S; spec = Incomplete _ } as s') ->" tag);
        printf2 fmt              "    s'.spec <- Complete { size = %zu; align = %zu }\n" ssize salign;
    | `Union (tag, typedef) ->
        let usize fmt = sizeof fmt typedef
        and ualign fmt = alignmentof fmt typedef in
        puts fmt (Printf.sprintf "  | Union ({ utag = %S; uspec = None } as s') ->" tag);
        printf2 fmt              "    s'.uspec <- Some { size = %zu; align = %zu }\n" usize ualign;
    | `Other -> 
      raise (Unsupported "Sealing a non-structured type")
  in
  cases fmt specs
    ["";
     "let rec seal : type a. a typ -> unit = function"]
    ~case
    ["  | Struct { tag; spec = Complete _ } ->";
     "    raise (ModifyingSealedType tag)";
     "  | Union { utag; uspec = Some _ } ->";
     "    raise (ModifyingSealedType utag)";
     "  | View { ty } -> seal ty";
     "  | _ ->";
     "    raise (Unsupported \"Sealing a non-structured type\")";
     ""]

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
        (Format.asprintf "Ctypes_static.Primitive %a"
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
      printf1 fmt
        (Format.asprintf "  | %s, %S ->@\n    %s\n" p name e)
        (fun fmt -> Format.fprintf fmt "v");
      Format.fprintf fmt "@]@\n}@\n"
  in
  cases fmt consts
    ["type 'a const = 'a";
     "let constant (type t) name (t : t typ) : t = match t, name with"]
    ~case
    ["  | _, s -> failwith (\"unmatched constant: \"^ s)"] 
    

let write_enums fmt enums =
  let case name =
    printf1 fmt
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
  List.iter (puts fmt) mlprologue;
  write_field fmt fields;
  write_seal fmt structures;
  write_consts fmt consts;
  write_enums fmt enums

let bindings (module B: BINDINGS)  =
  let fields = ref []
  and structures = ref []
  and consts = ref []
  and enums = ref [] 
  in
  let module M =
    B(struct
       include Ctypes
       open Ctypes_static
       let rec field' : type a s r. string -> s typ -> string -> a typ -> (a, r) field =
         fun structname s fname ftype -> match s with
         | Struct { tag } ->
           fields := (`Struct (tag, structname), fname) :: !fields;
           { ftype; foffset = -1; fname}           
         | Union { utag } ->
           fields := (`Union (utag, structname), fname) :: !fields;
           { ftype; foffset = -1; fname}           
         | View { ty } -> 
           field' structname ty fname ftype
         | _ -> raise (Unsupported "Adding a field to non-structured type")

       let field s fname ftype = field' (Ctypes.string_of_typ s) s fname ftype

       let rec seal' : type s. string -> s typ -> unit =
         fun structname -> function
         | Struct { tag } ->
           structures := `Struct (tag, structname) :: !structures
         | Union { utag } ->
           structures := `Union (utag, structname) :: !structures
         | View { ty } ->
           seal' structname ty
         | _ -> raise (Unsupported "Sealing a field to non-structured type")

       let seal ty = seal' (Ctypes.string_of_typ ty) ty

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
     end)
  in
  (!fields, !structures, !consts, !enums)
  
let write_c fmt (module B: BINDINGS) =
  let (fields,structures, consts, enums) = bindings (module B) in
  write_c fmt (fun fmt -> write_ml fmt fields structures consts enums)

module Easy =
struct
  let sprintf = Format.sprintf

  let c_function_name prefix =
    Format.sprintf "ctypes_%s_constant_value" prefix

  let unspace = Str.(global_replace (regexp_string " ") "_")

  let field_constructor = function
      `Struct (_, typename), fname -> sprintf "Struct_%s_offset_%s" (unspace typename) fname
    | `Union (_, typename), fname -> sprintf "Union_%s_offset_%s" (unspace typename) fname
  let struct_size_constructor = function
      `Struct (_, typename) -> sprintf "Struct_%s_size" (unspace typename)
    | `Union (_, typename) -> sprintf "Union_%s_size" (unspace typename)
  let struct_alignment_constructor = function
      `Struct (_, typename) -> sprintf "Struct_%s_alignment" (unspace typename)
    | `Union (_, typename) -> sprintf "Union_%s_alignment" (unspace typename)
  let constant_constructor (name, ty) =
    sprintf "Constant_%s" name
  let enum_constructor name =
    sprintf "Enum_%s" name

  let iter ~field ~structure ~constant ~enum (fields, structures, consts, enums) =
    begin
      List.iter field fields;
      List.iter structure structures;
      List.iter constant consts;
      List.iter enum enums;
    end

  let rec constant_injector : type a. a typ -> Cstubs_c_language.cfunction =
    let open Ctypes_static in function
        Primitive p -> Cstubs_c_language.prim_inj p
      | View { ty } -> constant_injector ty
      | _ -> failwith "constant of non-primitive"

  let rec constant_type_path : type a. a typ -> Ctypes_path.path =
    let open Ctypes_static in function
        Primitive p -> Cstubs_public_name.ident_of_ml_prim
                         (Ctypes_primitive_types.ml_prim p)
      | View { ty } -> constant_type_path ty
      | _ -> failwith "constant of non-primitive"

  module C =
  struct
    let cprologue = [
      "#if !__USE_MINGW_ANSI_STDIO && (defined(__MINGW32__) || defined(__MINGW64__))";
      "#define __USE_MINGW_ANSI_STDIO 1";
      "#endif";
      "";
      "#include <stddef.h>";
      "#include <caml/fail.h>";
      "#include \"ctypes_cstubs_internals.h\"";
      "";
    ]
    
    let write_enum fmt items =
      let pr s = Format.fprintf fmt s in
      pr "enum@ ctypes_constant@ {@\n";
      iter items
        ~field:(fun f -> pr "@ @ %s,@\n" (field_constructor f))
        ~structure:(fun f ->
            pr "@ @ %s,@\n" (struct_size_constructor f);
            pr "@ @ %s,@\n" (struct_alignment_constructor f))
        ~constant:(fun f ->
            pr "@ @ %s,@\n" (constant_constructor f))
        ~enum:(fun f ->
            pr "@ @ %s,@\n" (enum_constructor f));
      pr "@ @ Constant_count@\n";
      pr "};@\n@\n"

    let typename = function
        `Struct (_, typename) -> typename
      | `Union (_, typename)  -> typename

    let write_c fmt prefix items =
      let pr s = Format.fprintf fmt s in
      List.iter (pr "@[%s@]@\n") cprologue;
      write_enum fmt items;
      let funname = c_function_name prefix in
      pr "value@ %s(value c)@\n" funname;
      pr "{@\n";
      pr "  switch(Int_val(c))@\n  {@[<2>@ ";
      iter items
        ~field:(fun ((ty, f) as fld) -> pr "@[<2>case %s:@ return Val_int(%a);@]@\n"
                   (field_constructor fld)
                   offsetof (typename ty, f))
        ~structure:(fun s ->
            pr "@[<2>case %s:@ return Val_int(%a);@]@\n" (struct_size_constructor s)
              sizeof (typename s);
            pr "@[<2>case %s:@ return Val_int(%a);@]@\n" (struct_alignment_constructor s)
              alignmentof (typename s))
        ~constant:(fun ((name, Ctypes_static.BoxedType ty) as f) ->
            pr "@[<2>case %s:@ return %s(%s);@]@\n" (constant_constructor f)
              (constant_injector ty).Cstubs_c_language.fname name)
        ~enum:(fun e ->
            pr "@[<2>case %s:@ return " (enum_constructor e);
            pr " Val_int(CTYPES_CLASSIFY_ARITHMETIC_TYPE(enum %s));@]@\n" e);
      pr "default: @[caml_invalid_argument(%S);@]@]@\n  }@\n" funname;
      pr "}@\n@."
  end

  module ML =
  struct
    (*
      external constant_value : 'a ctypes_constant -> 'a
        = "ctypes_${prefix}_constant_value"
    *)
    let write_external fmt prefix =
      let pr s = Format.fprintf fmt s in
      pr "external constant_value : 'a ctypes_constant -> 'a@\n";
      pr "  = \"ctypes_%s_constant_value\"@\n@\n" prefix

    (*
     type _ ctypes_constant =
      | Enum_tag1 : Ctypes_static.arithmetic ctypes_constant
      | Enum_tag2 : Ctypes_static.arithmetic ctypes_constant
        ...
      | Enum_tagn : Ctypes_static.arithmetic ctypes_constant
      | Constant_k1 : c1 ctypes_constant
      | Constant_k2 : c2 ctypes_constant
      | ...
      | Constant_kn : cn ctypes_constant
      | Struct_field_offset : int ctypes_constant
      | Struct_field_offset : int ctypes_constant
      | Struct_tag_size : int ctypes_constant
      | Struct_tag_alignment : int ctypes_constant
      | Union_tag_size : int ctypes_constant
      | Union_tag_alignment : int ctypes_constant
    *)
    let write_type fmt specs =
      let pr s = Format.fprintf fmt s in
      pr "type _ ctypes_constant =@\n@[";
      iter specs
        ~field:(fun f -> pr "@ @ | %s : int ctypes_constant@\n"
                   (field_constructor f))
        ~structure:(fun s ->
            pr "@ @ | %s: int ctypes_constant@\n" (struct_size_constructor s);
            pr "@ @ | %s: int ctypes_constant@\n" (struct_alignment_constructor s))
        ~constant:(fun ((_, Ctypes_static.BoxedType ty) as c) ->
            let open Cstubs_c_language in
            pr "@ @ | %s: %a ctypes_constant@\n"
              (constant_constructor c) 
              Ctypes_path.format_path (constant_type_path ty))
        ~enum:(fun e ->
            pr "@ @ | %s: Ctypes_static.arithmetic ctypes_constant@\n"
              (enum_constructor e));
      pr "@]@\n"

    (*
     let field : ... =
       fun s fname ftype -> match s, fname with
        | Struct ({ tag = "tag_1"} as s'), "field_1" ->
          let f = {ftype; fname; foffset = constant_value Struct_field_offset }
           in begin s'.fields <- BoxedField f :: s'.fields; f end
        | ...
        | Union ({ utag = "tag_2"} as s'), "field_2" ->
          let f = {ftype; fname; foffset = constant_value Struct_field_offset }
           in begin s'.ufields <- BoxedField f :: s'.ufields; f end
        | _ -> raise (Unsupported "Adding a field to non-structured type")
    *)
    let write_field fmt fields =
      let pr s = Format.fprintf fmt s in
      let write_field = function
        | `Struct (tag, _), name as f ->
          pr "   | Struct ({ tag = %S} as s'), %S ->@\n" tag name;
          pr "     let f = {ftype; fname;@\n";
          pr "              foffset = constant_value %s }@\n"
            (field_constructor f);
          pr "       in begin s'.fields <- BoxedField f :: s'.fields; f end@\n";
        | `Union (tag, _), name as f ->
          pr "   | Union ({ utag = %S} as s'), %S ->@\n" tag name;
          pr "     let f = {ftype; fname;@\n";
          pr "              foffset = constant_value %s }@\n"
            (field_constructor f);
          pr "       in begin s'.ufields <- BoxedField f :: s'.ufields; f end@\n" in
      pr "let field : type a s. 't typ -> string -> a typ ->@\n";
      pr "  (a, ((s, [<`Struct | `Union]) structured as 't)) field =@\n";
      pr "  fun (type k) (s : (_, k) structured typ) fname ftype -> @\n";
      pr " @[match s, fname with@\n";
      List.iter write_field fields;
      pr "   | _ -> raise (Unsupported \"Adding a field to non-structured type\")@]@\n@\n"

    (*
     let seal : ... = function
        | Struct ({ tag = "tag"; spec = Incomplete _} as s) ->
            s.spec <- Complete  { size = constant_value Struct_tag_size;
                                  align = constant_value Struct_tag_alignment }
        | ...
        | Union ({ utag = "tag"; uspec = Incomplete _} as s) ->
            s.uspec <- Some  { size = constant_value Union_tag_size;
                               align = constant_value Union_tag_alignment }
        | Struct { tag; spec = Complete _; } ->
          raise (ModifyingSealedType tag)
        | Union { utag; uspec = Some _; } ->
          raise (ModifyingSealedType utag)
        | _ ->
          raise (Unsupported "Sealing a non-structured type")
    *)
    let write_seal fmt structures =
      let pr s = Format.fprintf fmt s in
      let case = function
        `Struct (tag, _) as s ->
          pr " | Struct ({ tag = %S; spec = Incomplete _} as s) ->@\n" tag;
          pr "   s.spec <- Complete  { size = constant_value %s;@\n"
            (struct_size_constructor s);
          pr "                         align = constant_value %s }@\n"
            (struct_alignment_constructor s)
        | `Union (tag, _) as s ->
          pr " | Union ({ utag = %S; uspec = None} as u) ->@\n" tag;
          pr "   u.uspec <- Some { size = constant_value %s;@\n"
            (struct_size_constructor s);
          pr "                     align = constant_value %s }@\n"
            (struct_alignment_constructor s)
      in
      pr "let seal (type t) (t : (_, t) structured typ) = match t with@\n";
      List.iter case structures;
      pr " | Struct { tag; spec = Complete _; } ->@\n";
      pr "   raise (ModifyingSealedType tag)@\n";
      pr " | Union { utag; uspec = Some _; } ->@\n";
      pr "   raise (ModifyingSealedType utag)@\n";
      pr " | _ ->@\n";
      pr "   raise (Unsupported \"Sealing a non-structured type\")@\n@\n"


    (*
     type 'a const = 'a
     let rec constant : ... = fun name t -> match n, t with
      |  name, View { read; ty } ->
          read (constant name ty)
      | "name", Primitive s ->
          constant_value Constant_ki
      | _ -> failwith "constant of non-primitive"
    *)
    let write_constant fmt consts =
      let pr s = Format.fprintf fmt s in
      let open Ctypes_static in
      let rec constant_case ((name, BoxedType ty) as t) = match ty with
        | View { ty } -> constant_case (name, BoxedType ty)
        | Primitive p ->
          pr " | %S, Primitive %a -> constant_value %s@\n"
            name Ctypes_path.format_path
            (Cstubs_public_name.constructor_cident_of_prim p)
            (constant_constructor t)
        | _ -> failwith "constant of non-primitive"
      in
      pr "type 'a const = 'a@\n";
      pr "let rec constant : type t. string -> t typ -> t =@\n";
      pr "  fun name t -> match name, t with@\n";
      pr " | name, View { read; ty } -> read (constant name ty)@\n";
      List.iter constant_case consts;
      pr " | c, Primitive _ -> failwith (Printf.sprintf \"unknown constant: %%s\" c)@\n";
      pr " | _ -> failwith (\"constant of non-primitive\")";
      pr "@\n@\n"

    (*
     let enum_type : string -> arithmetic = function
       "tag1" -> constant_value Enum_tag1
     | "tag2" -> constant_value Enum_tag2
     | ...
     | "tagn" -> constant_value Enum_tagn
     | n -> failwithk "unexpected enum name: %s" n

     let enum name ?unexpected alist ->
        Cstubs_internals.build_enum_type (enum_type name) ?unexpected alist
    *)
    let write_enums fmt enums =
      let pr s = Format.fprintf fmt s in
      pr "let enum_type : string -> Ctypes_static.arithmetic = function@\n";
      ListLabels.iter enums
        ~f:(fun e -> pr " | %S -> constant_value %s@\n" e (enum_constructor e));
      pr " | e -> failwith (Printf.sprintf \"unexpected enum name: %%s\" e)@\n@\n";
      pr "let enum name ?unexpected alist =@\n";
      pr "  Cstubs_internals.build_enum_type name (enum_type name) ?unexpected alist@\n"

    let mlprologue = mlprologue

    let write_ml fmt prefix ((fields, structures, consts, enums) as specs) =
      List.iter (Format.fprintf fmt "@[%s@]@\n") mlprologue;
      write_type fmt specs;
      write_external fmt prefix;
      write_field fmt fields;
      write_seal fmt structures;
      write_constant fmt consts;
      write_enums fmt enums
  end

  let write_c fmt ~prefix b =
    C.write_c fmt prefix (bindings b)

  let write_ml fmt ~prefix b =
    ML.write_ml fmt prefix (bindings b)
end
