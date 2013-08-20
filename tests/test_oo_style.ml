(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open OUnit
open Ctypes


let testlib = Dl.(dlopen ~filename:"clib/test_functions.so" ~flags:[RTLD_NOW])


(* 
   Establish a hierarchy of "classes", create some "objects" and call some
   "methods".
*)
let test_oo_hierarchy () =
  let module M = struct
    let cast base p = from_voidp base (to_voidp p)

    (* We'll build part of the hierarchy in C and part in OCaml.

          animal
          ^    ^
          |    |
     chorse   camel
    *)

    (** Create the base class and its method table **)
    type animal and animal_methods

    let animal_methods : animal_methods structure typ = structure "animal methods"
    and animal : animal structure typ = structure "animal" 

    (* class layout (vtable pointer, no instance variables) *)
    let animal_vtable = field animal "animal_vtable" (ptr animal_methods)
    let () = seal animal
      
    (* method table layout (two virtual methods) *)
    let (-:) ty label = field animal_methods label ty
    let say = Foreign.funptr (ptr animal @-> returning string)      -: "say"
    let identify = Foreign.funptr (ptr animal @-> returning string) -: "identify"
    let () = seal animal_methods

    let call_say cinstance =
      !@((getf (!@cinstance) animal_vtable) |-> say) cinstance

    let call_identify cinstance =
      !@((getf (!@cinstance) animal_vtable) |-> identify) cinstance

    (* constructor *)
    class animalc ~cinstance = object
      method say : string = call_say cinstance
      method identify : string = call_identify cinstance
      method cinstance = cinstance
    end
      
    (** Create a sub class and its method table **)
    type camel and camel_methods
    let camel_methods : camel_methods structure typ = structure "camel methods"
    and camel : camel structure typ = structure "camel"

    (* class layout (vtable pointer, one instance variable) *)
    let (-:) ty label = field camel label ty 
    let camel_vtable = ptr camel_methods -: "camel_vtable"
    let nhumps       = int               -: "nhumps"
    let () = seal camel

    (* method table layout (one additional virtual method) *) 
    let (-:) ty label = field camel_methods label ty 
    let _     = animal_methods                       -: "_"
    let humps = Foreign.funptr (ptr camel @-> returning int) -: "humps"
    let () = seal camel_methods

    let call_humps cinstance =
      !@((getf (!@cinstance) camel_vtable) |-> humps) cinstance

    (* constructor *)
    class camelc ~cinstance = object
      inherit animalc ~cinstance:(cast animal cinstance)
      method humps : int = call_humps cinstance
    end
      
    let camel_vtable_singleton = make camel_methods
    let () = begin
      let vt = camel_vtable_singleton in
      let base_vt = !@(cast animal_methods (addr vt)) in
      (* say *)
      setf base_vt say (fun animal -> "humph");

      (* identify *)
      setf base_vt identify (fun animal ->
        let n = call_humps (cast camel animal) in
        Printf.sprintf "%d-hump camel" n);

      (* humps *)
      setf vt humps (fun camel -> !@(camel |-> nhumps))
    end

    let new_camel ~humps =
      let c = make camel in begin
        setf c camel_vtable (addr camel_vtable_singleton);
        setf c nhumps humps
      end;
      new camelc ~cinstance:(addr c)

    let check_name = Foreign.foreign "check_name" ~from:testlib
      (ptr animal @-> string @-> returning int)

    let () =
      let c = new_camel ~humps:3 in begin
        (* Test that we can call a virtual method in an OCaml-created subclass
           from C *)
        assert_equal 1 (check_name (cast animal c#cinstance) "3-hump camel");
        
        (* Test that we can call virtual methods in an OCaml-created subclass
           from OCaml *)
        assert_equal c#identify "3-hump camel";
        assert_equal c#say "humph";
        assert_equal c#humps 3;
      end

    (* Test that we can call a virtual method in a C-created subclass from
       OCaml *)
    type colour = White | Red | Black | Pale
    let colour_num = function
    White -> 0 | Red -> 1 | Black -> 2 | Pale -> 3

    let new_chorse = Foreign.foreign "new_chorse" ~from:testlib
      (int @-> returning (ptr animal)) 

    class chorse ~colour =
    object
      inherit animalc (new_chorse (colour_num colour))
    end

    let () =
      let red_horse = new chorse ~colour:Red
      and pale_horse = new chorse ~colour:Pale in begin
        assert_equal "red horse" red_horse#identify;
        assert_equal "pale horse" pale_horse#identify;
        assert_equal "neigh" pale_horse#say;
      end
  end in ()


let suite = "OO-style tests" >:::
  ["OO style"
    >:: test_oo_hierarchy;
  ]


let _ =
  run_test_tt_main suite
