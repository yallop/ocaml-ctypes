(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Cstubs public interface. *)

module type FOREIGN =
sig
  type 'a fn
  val foreign : string -> ('a -> 'b) Ctypes.fn -> ('a -> 'b) fn
  val view_invoke : ('a -> 'b) fn -> 'a -> 'b
end

module type FOREIGN' = FOREIGN with type 'a fn = unit

module type BINDINGS = functor (F : FOREIGN') -> sig end

let gen_c prefix fmt : (module FOREIGN') =
  (module
   struct
     let counter = ref 0
     let var prefix name = incr counter;
       Printf.sprintf "%s_%d_%s" prefix !counter name
     type 'a fn = unit
     let foreign cname fn =
       Cstubs_generate_c.fn ~cname ~stub_name:(var prefix cname) fmt fn
     let view_invoke _ _ =
       failwith "must not invoke bindings during stub generation"
   end)

type bind = Bind : string * string * ('a -> 'b) Ctypes.fn -> bind

let write_foreign fmt bindings =
  Format.fprintf fmt
    "type 'a fn = 'a@\n@\n";
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

let write_invoke fmt =
  Format.fprintf fmt "let view_invoke f v = f v@."

let gen_ml prefix fmt : (module FOREIGN') * (unit -> unit) =
  let bindings = ref []
  and counter = ref 0 in
  let var prefix name = incr counter;
    Printf.sprintf "%s_%d_%s" prefix !counter name in
  (module
   struct
     type 'a fn = unit
     let foreign cname fn =
       let name = var prefix cname in
       bindings := Bind (cname, name, fn) :: !bindings;
       Cstubs_generate_ml.extern ~stub_name:name ~external_name:name fmt fn
     let view_invoke _ _ =
       failwith "must not invoke bindings during stub generation"
   end),
  fun () -> write_foreign fmt !bindings; write_invoke fmt

let write_c fmt ~prefix (module B : BINDINGS) =
  Format.fprintf fmt
    "#include \"ctypes/cstubs_internals.h\"@\n@\n";
  let module M = B((val gen_c prefix fmt)) in ()

let write_ml fmt ~prefix (module B : BINDINGS) =
  let foreign, finally = gen_ml prefix fmt in
  let () = Format.fprintf fmt "module CI = Cstubs_internals@\n@\n" in
  let module M = B((val foreign)) in
  finally ()
