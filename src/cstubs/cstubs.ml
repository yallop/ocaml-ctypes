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
  type 'a return
  val (@->) : 'a Ctypes.typ -> 'b fn -> ('a -> 'b) fn
  val returning : 'a Ctypes.typ -> 'a return fn

  type 'a result
  val foreign : string -> ('a -> 'b) fn -> ('a -> 'b) result
  val foreign_value : string -> 'a Ctypes.typ -> 'a Ctypes.ptr result
end

module type FOREIGN' = FOREIGN with type 'a result = unit

module type BINDINGS = functor (F : FOREIGN') -> sig end

let gen_c prefix fmt : (module FOREIGN') =
  (module
   struct
     let counter = ref 0
     let var prefix name = incr counter;
       Printf.sprintf "%s_%d_%s" prefix !counter name
     type 'a fn = 'a Ctypes.fn
     type 'a return = 'a
     type 'a result = unit
     let foreign cname fn =
       Cstubs_generate_c.fn ~cname ~stub_name:(var prefix cname) fmt fn
     let foreign_value cname typ =
       Cstubs_generate_c.value ~cname ~stub_name:(var prefix cname) fmt typ
     let returning = Ctypes.returning
     let (@->) = Ctypes.(@->)
   end)

type bind = Bind : string * string * ('a -> 'b) Ctypes.fn -> bind
type val_bind = Val_bind : string * string * 'a Ctypes.typ -> val_bind

let write_foreign fmt bindings val_bindings =
  Format.fprintf fmt
    "type 'a fn = 'a Ctypes.fn@\n";
  Format.fprintf fmt
    "type 'a result = 'a@\n";
  Format.fprintf fmt
    "type 'a return = 'a@\n";
  Format.fprintf fmt
    "let returning = Ctypes.returning@\n";
  Format.fprintf fmt
    "let (@@->) f p = Ctypes.(f @@-> p)@\n";
  Format.fprintf fmt
    "let foreign : type a b. string -> (a -> b) Ctypes.fn -> (a -> b) =@\n";
  Format.fprintf fmt
    "  fun name t -> match t, name with@\n@[<v>";
  ListLabels.iter bindings
    ~f:(fun (Bind (stub_name, external_name, fn)) ->
      Cstubs_generate_ml.case ~stub_name ~external_name fmt fn);
  Format.fprintf fmt "@[<hov 2>@[|@ _,@ s@ ->@]@ ";
  Format.fprintf fmt
    " @[Printf.ksprintf@ failwith@ \"No match for %%s\" s@]@]@]@.@\n";
  Format.fprintf fmt
    "@\n";
  Format.fprintf fmt
    "let foreign_value : type a b. string -> a Ctypes.typ -> a Ctypes.ptr =@\n";
  Format.fprintf fmt
    "  fun name t -> match t, name with@\n@[<v>";
  ListLabels.iter val_bindings
    ~f:(fun (Val_bind (stub_name, external_name, typ)) ->
      Cstubs_generate_ml.val_case ~stub_name ~external_name fmt typ);
  Format.fprintf fmt "@[<hov 2>@[|@ _,@ s@ ->@]@ ";
  Format.fprintf fmt
    " @[Printf.ksprintf@ failwith@ \"No match for %%s\" s@]@]@]@.@\n"

let gen_ml prefix fmt : (module FOREIGN') * (unit -> unit) =
  let bindings = ref []
  and val_bindings = ref []
  and counter = ref 0 in
  let var prefix name = incr counter;
    Printf.sprintf "%s_%d_%s" prefix !counter name in
  (module
   struct
     type 'a fn = 'a Ctypes.fn
     type 'a return = 'a
     let (@->) = Ctypes.(@->)
     let returning = Ctypes.returning

     type 'a result = unit
     let foreign cname fn =
       let name = var prefix cname in
       bindings := Bind (cname, name, fn) :: !bindings;
       Cstubs_generate_ml.extern ~stub_name:name ~external_name:name fmt fn
     let foreign_value cname typ =
       let name = var prefix cname in
       Cstubs_generate_ml.extern ~stub_name:name ~external_name:name fmt
         Ctypes.(void @-> returning (ptr void));
       val_bindings := Val_bind (cname, name, typ) :: !val_bindings
     let returning = Ctypes.returning
   end),
  fun () ->
    write_foreign fmt !bindings !val_bindings

type concurrency_policy = Sequential

let sequential = Sequential

let write_c ?(concurrency=Sequential) fmt ~prefix (module B : BINDINGS) =
  Format.fprintf fmt
    "#include \"ctypes_cstubs_internals.h\"@\n@\n";
  let module M = B((val gen_c prefix fmt)) in ()

let write_ml ?(concurrency=Sequential) fmt ~prefix (module B : BINDINGS) =
  let foreign, finally = gen_ml prefix fmt in
  let () = Format.fprintf fmt "module CI = Cstubs_internals@\n@\n" in
  let module M = B((val foreign)) in
  finally ()

module Types = Cstubs_structs
