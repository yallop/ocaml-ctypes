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
end

module type FOREIGN' = FOREIGN with type 'a fn = unit

module type BINDINGS = functor (F : FOREIGN') -> sig end

let gen_c prefix fmt : (module FOREIGN') =
  (module
   struct
     type 'a fn = unit
     let foreign cname fn =
       Cstubs_generate_c.fn ~cname ~stub_name:(prefix ^ cname) fmt fn
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

let gen_ml prefix fmt : (module FOREIGN') * (unit -> unit) =
  let bindings = ref []
  and counter = ref 0 in
  let var prefix name = incr counter;
    Printf.sprintf "%s_%d_%s" prefix !counter name in
  (module
   struct
     type 'a fn = unit
     let foreign cname fn =
       let external_name = var prefix cname 
       and stub_name = prefix ^ cname in
       bindings := Bind (cname, external_name, fn) :: !bindings;
       Cstubs_generate_ml.extern ~stub_name ~external_name fmt fn
   end),
  fun () -> write_foreign fmt !bindings

let write_c fmt ~prefix (module B : BINDINGS) =
  let module M = B((val gen_c prefix fmt)) in ()

let write_ml fmt ~prefix (module B : BINDINGS) =
  let foreign, finally = gen_ml prefix fmt in
  let () = Format.fprintf fmt "module CI = Cstubs_internals@\n@\n" in
  let module M = B((val foreign)) in
  finally ()

module type CONSTANT =
sig
  type 'a t
  val constant : 'a Ctypes.typ -> string -> 'a t
end

module type CONSTANT' = CONSTANT with type 'a t = unit

module type CONSTANT_BINDINGS = functor (C : CONSTANT') -> sig end

let gen_constant_c fmt : (unit -> unit) * (module CONSTANT') * (unit -> unit) =
  let initially () = 
    Format.pp_print_string fmt "
#include <stdio.h>
int main() {
"
  and finally () =
    Format.pp_print_string fmt "
return 0;
}
"
  and body : (module CONSTANT') =
    (module
     struct
       type 'a t = unit
       let constant typ name =
       (* Cstubs_generate_c.const fmt name typ *)
         assert false
     end) in
  initially, body, finally
      

let write_constant_c fmt (module B : CONSTANT_BINDINGS) =
  let initially, body, finally = gen_constant_c fmt in
  initially ();
  let module M = B((val body)) in 
  finally ()
