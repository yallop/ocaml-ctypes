external add_slow: int -> int = "add_int_slow"
external add_fast: int -> int = "add_int_fast"
external add_fast_noalloc: int -> int = "add_int_fast" "noalloc"

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
   printf "direct_add_slow        : %.4f\n" (time add_slow);
   printf "direct_add_fast        : %.4f\n" (time add_fast);
   printf "direct_add_fast_noalloc: %.4f\n" (time add_fast_noalloc);
   ()
