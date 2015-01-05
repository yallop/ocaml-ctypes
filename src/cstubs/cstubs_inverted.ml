(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Cstubs_inverted public interface. *)

module type INTERNAL =
sig
  val enum : (string * int64) list -> 'a Ctypes.typ -> unit
  val structure : _ Ctypes.structure Ctypes.typ -> unit
  val union : _ Ctypes.union Ctypes.typ -> unit

  val internal : string -> ('a -> 'b) Ctypes.fn -> ('a -> 'b) -> unit
end

module type BINDINGS = functor (F : INTERNAL) -> sig end

type fn_info = Fn : string * (_ -> _) Ctypes.fn -> fn_info
type ty_info = Ty : string * _ Ctypes.typ -> ty_info
type ty = Ty : _ Ctypes.typ -> ty
type enum = Enum : (string * int64) list * _ Ctypes.typ -> enum

let collector () : (module INTERNAL) * (unit -> (ty list * enum list * fn_info list)) =
  let function_names = ref [] in
  let structures = ref [] in
  let enums = ref [] in
  let push x xs = xs := x :: !xs in
  ((module
    struct
      type 'a fn = unit
      let enum constants typ = push (Enum (constants, typ)) enums
      let structure typ = push (Ty typ) structures
      let union typ = push (Ty typ) structures
      let internal name fn _ = push (Fn (name, fn)) function_names
    end),
   (fun () -> 
     (List.rev !structures,
      List.rev !enums,
      List.rev !function_names)))

let format_enum_values fmt infos =
  List.iter (fun (Fn (n, _)) -> Format.fprintf fmt "@[fn_%s,@]@ " n) infos

let c_prologue fmt register infos =
  Format.fprintf fmt "#include <caml/memory.h>@\n";
  Format.fprintf fmt "#include <caml/callback.h>@\n";
  Format.fprintf fmt "#include \"ctypes/cstubs_internals.h\"@\n@\n";
  Format.fprintf fmt "enum functions@\n{@[<v 2>@ %afn_count@]@\n};"
    format_enum_values infos;
  Format.fprintf fmt "@\n
/* A table of OCaml \"callbacks\". */
static value functions[fn_count];

/* Record a value in the callback table. */
value %s(value i, value v)
{
  CAMLparam2(i, v);
  functions[Int_val(i)] = v;
  caml_register_global_root(&functions[Int_val(i)]);
  CAMLreturn (Val_unit);
}@\n" register

let c_function fmt (Fn (stub_name, fn)) : unit =
  Cstubs_generate_c.inverse_fn ~stub_name fmt fn

let gen_c fmt register infos =
  begin
    c_prologue fmt register infos;
    List.iter (c_function fmt) infos
  end

let c_declaration fmt (Fn (stub_name, fn)) : unit =
  Cstubs_generate_c.inverse_fn_decl ~stub_name fmt fn

let write_structure_declaration fmt (Ty ty) =
  Format.fprintf fmt "@[%a@];@\n@\n" (fun ty -> Ctypes.format_typ ty) ty

let write_enum_declaration fmt (Enum (constants, ty)) =
  Format.fprintf fmt "@[%a@ {@\n@[<v 2>@\n" (fun ty -> Ctypes.format_typ ty) ty;
  let last = List.length constants - 1 in
  List.iteri
    (fun i (name, value) -> 
       (* Trailing commas are not allowed. *)
       if i < last
       then Format.fprintf fmt "@[%s@ =@ %Ld,@]@\n" name value
       else Format.fprintf fmt "@[%s@ =@ %Ld@]@\n" name value)
    constants;
  Format.fprintf fmt "@]@]@\n};@\n@\n"

let write_c fmt ~prefix (module B : BINDINGS) : unit =
  let register = prefix ^ "_register" in
  let m, infos = collector () in
  let module M = B((val m)) in
  let _, _, functions = infos () in
  gen_c fmt register functions;
  Format.fprintf fmt "@."

let write_c_header fmt ~prefix (module B : BINDINGS) : unit =
  let m, infos = collector () in
  let module M = B((val m)) in
  let (structures, enums, functions) = infos () in
  List.iter (write_enum_declaration fmt) enums;
  List.iter (write_structure_declaration fmt) structures;
  List.iter (c_declaration fmt) functions;
  Format.fprintf fmt "@."

let gen_ml fmt register (infos : fn_info list) : unit =
  Format.fprintf fmt 
    "type 'a fn = 'a@\n@\n";
  Format.fprintf fmt
    "module CI = Cstubs_internals@\n@\n";
  Format.fprintf fmt 
    "type 'a name = @\n";
  ListLabels.iter infos
    ~f:(fun (Fn (n, fn)) ->
      Cstubs_generate_ml.constructor_decl (Printf.sprintf "Fn_%s" n) fn fmt);
  Format.fprintf fmt
    "@\n";
  Format.fprintf fmt
    "@[<h>external register_value : 'a name -> 'a fn -> unit =@\n@ @ \"%s\"@]@\n@\n"
    register;
  Format.fprintf fmt
    "@[<h>let internal : ";
  Format.fprintf fmt
    "@[type a b.@ @[string -> (a -> b) Ctypes.fn -> (a -> b) -> unit@]@]@ =@\n";
  Format.fprintf fmt
    "fun name fn f -> match name, fn with@\n@[";
  ListLabels.iter infos
    ~f:(fun (Fn (n, fn)) ->
      Cstubs_generate_ml.inverse_case ~register_name:"register_value"
        ~constructor:(Printf.sprintf "Fn_%s" n) n fmt fn);
  Format.fprintf fmt
    "| _ -> failwith (\"Linking mismatch on name: \" ^ name)@]@]@]@\n@\n";
  Format.fprintf fmt
    "let enum _ _ = () and structure _ = () and union _ = ()@."

let write_ml fmt ~prefix (module B : BINDINGS) : unit =
  let register = prefix ^ "_register" in
  let m, infos = collector () in
  let module M = B((val m)) in
  let _, _, functions = infos () in
  gen_ml fmt register functions
