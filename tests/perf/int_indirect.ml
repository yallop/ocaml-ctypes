open Ctypes.Ffi.C
open Type
let add = foreign "add_stuff_up" (int @-> returning int)

open Time
open Printf 
let () =
   printf "indirect_add           : %.4f\n" (time add);
   ()
