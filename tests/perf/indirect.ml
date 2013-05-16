open Ctypes.Ffi.C
open Type
let add = foreign "add_stuff_up" (int @-> returning int)

let time f =
  let t1 = Unix.gettimeofday () in
  Gc.compact ();
  for i = 0 to 100000000 do
    ignore(f i)
  done;
  let t2 = Unix.gettimeofday () in
  t2 -. t1

open Printf 
let () =
   printf "indirect_add           : %.4f\n" (time add);
   ()
