(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Cstubs_inverted public interface. *)

module type INTERNAL =
sig
  val internal : string -> ('a -> 'b) Ctypes.fn -> ('a -> 'b) -> unit
end

module type BINDINGS = functor (F : INTERNAL) -> sig end

type fn_info = Fn : string * ('a -> 'b) Ctypes.fn -> fn_info
type ty_info = Ty : string * 'a Ctypes.typ -> ty_info
type ty = Ty : 'a Ctypes.typ -> ty

let collector () : (module INTERNAL) * (unit -> fn_info list) =
  let function_names = ref [] in
  ((module
    struct
      type 'a fn = unit
      let internal name fn _ =
        function_names := Fn (name, fn) :: !function_names
    end),
   fun () -> List.rev !function_names)

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

let write_c fmt ~prefix (module B : BINDINGS) : unit =
  let register = prefix ^ "_register" in
  let m, infos = collector () in
  let module M = B((val m)) in
  gen_c fmt register (infos ());
  Format.fprintf fmt "@."

let write_c_header fmt ~prefix (module B : BINDINGS) : unit =
  let m, infos = collector () in
  let module M = B((val m)) in
  List.iter (c_declaration fmt) (infos ());
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
    "| _ -> failwith (\"Linking mismatch on name: \" ^ name)@]@]@]@."

let write_ml fmt ~prefix (module B : BINDINGS) : unit =
  let register = prefix ^ "_register" in
  let m, infos = collector () in
  let module M = B((val m)) in
  gen_ml fmt register (infos ())
