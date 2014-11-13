(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Cstubs public interface. *)

module type FOREIGN =
sig
  type 'a fn (* The result type *)

  type 'a f   (* The abstract function type *)
  type 'a comp (* The computation type *)

  val returning : 'a Ctypes.typ -> 'a comp f
  val (@->) : 'a Ctypes.typ -> 'b f -> ('a -> 'b) f

  val foreign : string -> ('a -> 'b) f -> ('a -> 'b) fn
end

module type FOREIGN' = FOREIGN with type 'a fn = unit

module type BINDINGS = functor (F : FOREIGN') -> sig end

let gen_c prefix fmt : (module FOREIGN') =
  (module
   struct
     type 'a comp = 'a and 'a f = 'a Ctypes.fn
     let (returning, (@->)) = Ctypes.(returning, (@->))
     let counter = ref 0
     let var prefix name = incr counter;
       Printf.sprintf "%s_%d_%s" prefix !counter name
     type 'a fn = unit
     let foreign cname fn =
       Cstubs_generate_c.fn ~cname ~stub_name:(var prefix cname) fmt fn
   end)

type bind = Bind : string * string * ('a -> 'b) Ctypes.fn -> bind

let write_foreign fmt bindings =
  Format.fprintf fmt
    "type 'a fn = 'a@\n@\n";
  Format.fprintf fmt
    "type 'a comp = 'a and 'a f = 'a Ctypes.fn@\n";
  Format.fprintf fmt
    "let (returning, (@@->)) = Ctypes.(returning, (@@->))@\n@\n";
  Format.fprintf fmt
    "let foreign : type a b. string -> (a -> b) Ctypes.fn -> (a -> b) =@\n";
  Format.fprintf fmt
    "  fun name t -> match name, t with@\n@[<v>";
  ListLabels.iter bindings
    ~f:(fun (Bind (stub_name, external_name, fn)) ->
      Cstubs_generate_ml.case ~stub_name ~external_name fmt fn);
  Format.fprintf fmt "@[<hov 2>@[|@ s,@ _@ ->@]@ ";
  Format.fprintf fmt " @[@[Printf.fprintf@ stderr@ \"No match for %%s\" s@];";
  Format.fprintf fmt "@ @[assert false@]@]@]@]@."

let write_foreign_lwt fmt bindings =
  Format.fprintf fmt
    "type 'a fn = 'a@\n@\n";
  Format.fprintf fmt
    "type 'a comp = 'a Lwt.t@\n";
  Format.fprintf fmt
    "module CI = struct include Cstubs_internals@\n";
  Format.fprintf fmt
    "  type _ f =@\n";
  Format.fprintf fmt
    "  | Returns  : 'a Ctypes.typ   -> 'a Lwt.t f@\n";
  Format.fprintf fmt
    "  | Function : 'a Ctypes.typ * 'b f  -> ('a -> 'b) f@\n";
  Format.fprintf fmt
    "end@\n";
  Format.fprintf fmt
    "type 'a f = 'a CI.f@\n";
  Format.fprintf fmt
    "let (returning, (@@->)) = Ctypes.(returning, (@@->))@\n@\n";
  Format.fprintf fmt
    "let foreign : type a b. string -> (a -> b) f -> (a -> b) =@\n";
  Format.fprintf fmt
    "  fun name t -> match name, t with@\n@[<v>";
  ListLabels.iter bindings
    ~f:(fun (Bind (stub_name, external_name, fn)) ->
      Cstubs_generate_ml.case_lwt ~stub_name ~external_name fmt fn);
  Format.fprintf fmt "@[<hov 2>@[|@ s,@ _@ ->@]@ ";
  Format.fprintf fmt " @[@[Printf.fprintf@ stderr@ \"No match for %%s\" s@];";
  Format.fprintf fmt "@ @[assert false@]@]@]@]@."

let gen_ml prefix fmt : (module FOREIGN') * (unit -> unit) =
  let bindings = ref []
  and counter = ref 0 in
  let var prefix name = incr counter;
    Printf.sprintf "%s_%d_%s" prefix !counter name in
  (module
   struct
     type 'a comp = 'a and 'a f = 'a Ctypes.fn
     let (returning, (@->)) = Ctypes.(returning, (@->))
     type 'a fn = unit
     let foreign cname fn =
       let name = var prefix cname in
       bindings := Bind (cname, name, fn) :: !bindings;
       Cstubs_generate_ml.extern ~stub_name:name ~external_name:name fmt fn
   end),
  fun () -> write_foreign fmt !bindings

let gen_ml_lwt prefix fmt : (module FOREIGN') * (unit -> unit) =
  let bindings = ref []
  and counter = ref 0 in
  let var prefix name = incr counter;
    Printf.sprintf "%s_%d_%s" prefix !counter name in
  (module
   struct
     type 'a comp = 'a and 'a f = 'a Ctypes.fn
     let (returning, (@->)) = Ctypes.(returning, (@->))
     type 'a fn = unit
     let foreign cname fn =
       let name = var prefix cname in
       bindings := Bind (cname, name, fn) :: !bindings;
       Cstubs_generate_ml.extern ~stub_name:name ~external_name:name fmt fn
   end),
  fun () -> write_foreign_lwt fmt !bindings

let write_c fmt ~prefix (module B : BINDINGS) =
  Format.fprintf fmt
    "#include \"ctypes/cstubs_internals.h\"@\n@\n";
  let module M = B((val gen_c prefix fmt)) in ()

let write_ml fmt ~prefix (module B : BINDINGS) =
  let foreign, finally = gen_ml prefix fmt in
  let () = Format.fprintf fmt "module CI = Cstubs_internals@\n@\n" in
  let module M = B((val foreign)) in
  finally ()

let write_ml_lwt fmt ~prefix (module B : BINDINGS) =
  let foreign, finally = gen_ml_lwt prefix fmt in
  let module M = B((val foreign)) in
  finally ()

let write_enum fmt ~prefix (module B : BINDINGS) =
  let count = ref 0 in
  begin
    Format.fprintf fmt "enum@ %s_functions@ {@\n" prefix;
    let module M = B(struct
        type _ fn = unit
        type 'a comp = 'a and 'a f = 'a Ctypes.fn
        let (returning, (@->)) = Ctypes.(returning, (@->))
        let foreign name _ =
          incr count;
          Format.fprintf fmt "  @[%s_name,@]@\n" name
      end) in ();
    Format.fprintf fmt "  @[%s_count = %d@]@\n" prefix !count;
    Format.fprintf fmt "};@\n"
  end

let write_frame_structs fmt ~prefix (module B : BINDINGS) =
  let make_tag name = (* prefix ^"_"^  *)name ^"_frame" in
  let max_size = ref 0 in
  begin
    let module M = B(struct
        type 'a comp = 'a and 'a f = 'a Ctypes.fn
        let (returning, (@->)) = Ctypes.(returning, (@->))
        type _ fn = unit
        let foreign name fn =
          let tag = make_tag name in
          let s = Ctypes.structure tag in
          let _ = Ctypes.field s "fn_name" Ctypes.int in
          let rec loop : type a. int -> a Ctypes.fn -> unit =
            fun i -> function
              | Static.Function (Static.Void, fn) ->
                loop i fn
              | Static.Function (typ, fn) ->
                let _ = Ctypes.field s (Printf.sprintf "x%d" i) typ in
                loop (i + 1) fn
              | Static.Returns typ ->
                ignore (Ctypes.field s "return_value" typ)
          in
          loop 1 fn;
          Ctypes.seal s;
          max_size := max !max_size (Ctypes.sizeof s);
          Ctypes.format_typ fmt s;
          Format.fprintf fmt ";@\n";
      end) in ();
    Format.fprintf fmt "@[enum@ {@ %s_frame_size@ =@ %d};@]@\n" prefix !max_size
  end

let write_remote_dispatcher fmt ~prefix (module B : BINDINGS) =
  let write x = Format.fprintf fmt x in
  let rec arity : type a. a Static.fn -> int = function
    | Static.Returns _ -> 0
    | Static.Function (Static.Void, f) -> arity f
    | Static.Function (_, f) -> 1 + arity f
  in
  let write_call fmt (name, nargs) =
    write "@[%s(@[" name;
    for i = 1 to nargs do
      write "frame->x%d%(%)" i (if i <> nargs then ",@ " else "")
    done;
    write ")@]@]"
  in
  begin
    write "@[#include <assert.h>@]@\n";
    write "@[void@ %s_dispatch@[(int@ *call_buffer,@ void@ *arglock,@ void@ *retlock)@]@]@\n" prefix;
    write "{@[<2>@\n@[@[for@ (;;)@]@\n{@[<2>@\n";
    write "@[<2>cstubs_acquire_lock(arglock);@]@\n";
    write "@[switch@ ((enum@ %s_functions)@ *call_buffer)@]@\n{@[@\n" prefix;
    let module M = B(struct
        type 'a comp = 'a and 'a f = 'a Ctypes.fn
        let (returning, (@->)) = Ctypes.(returning, (@->))
        type _ fn = unit
        let foreign name typ =
          let frame_name = Printf.sprintf "%s_frame" name in
          write "@[case@ %s_name:@]@ {@ @[<2>@ @[" name;
          write "@[<2>@[struct@ %s@ *frame@]@ @[=@ (struct@ %s@ *)call_buffer;@]@]@\n"
            frame_name frame_name;
          write "frame->return_value = @[%a@];@\n" write_call (name, arity typ);
          write "cstubs_release_lock(retlock);@\n";
          write "continue;@\n@]@]}@\n"
      end) in ();
  write "@[default:@ assert(0);@]@\n";
  write "}@]@]@\n}@]@]\n}@."
  end

let write_remote_initializer_ml fmt ~prefix (module B : BINDINGS) =
  let pr x = Format.fprintf fmt x in 
  begin
    pr "external %s_initialize : unit -> unit = \"%s_initialize\"@\n"
      prefix prefix;
    pr "let () = %s_initialize ()@\n" prefix;
  end

let write_remote_initializer_c fmt ~prefix (module B : BINDINGS) =
  let pr x = Format.fprintf fmt x in 
  begin
    pr "void *%s_global_buffer = 0;@\n" prefix;
    pr "void *%s_global_arg_lock = 0;@\n" prefix;
    pr "void *%s_global_ret_lock = 0;@\n" prefix;
    pr "value %s_initialize(value unit)@\n" prefix;
    pr "{@\n";
    pr "  cstubs_initialize_shared_memory(%s_global_buffer);@\n" prefix;
    pr "  cstubs_initialize_arg_lock(%s_global_arg_lock);@\n" prefix;
    pr "  cstubs_initialize_ret_lock(%s_global_ret_lock);@\n" prefix;
    pr "  cstubs_start_remote_process();@\n";
    pr "  return Val_unit;@\n";
    pr "}@\n";
  end
