open Ctypes

let add =
  Ffi.C.(Type.(foreign "add_stuff_up" (int @-> returning int)))

let add_raw =
  Ffi_raw.(Types.(
    let callspec = allocate_callspec () in
    let arg_1_offset = Ffi_raw.add_argument callspec int in
    let () = prep_callspec callspec int in
    let dlfabs = Dl.dlsym "add_stuff_up" in
    fun x -> 
      call dlfabs callspec
        (write int ~offset:arg_1_offset x)
        (read int ~offset:0)
  ))

open Time
open Printf 
let () =
   printf "indirect_add           : %.4f\n" (time add);
   printf "indirect_add_raw       : %.4f\n" (time add_raw);
   ()
