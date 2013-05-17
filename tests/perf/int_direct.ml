external add_slow: int -> int = "add_int_slow"
external add_fast: int -> int = "add_int_fast"
external add_fast_noalloc: int -> int = "add_int_fast" "noalloc"

open Time
open Printf 
let () =
   printf "direct_add_slow        : %.4f\n" (time add_slow);
   printf "direct_add_fast        : %.4f\n" (time add_fast);
   printf "direct_add_fast_noalloc: %.4f\n" (time add_fast_noalloc);
   ()
