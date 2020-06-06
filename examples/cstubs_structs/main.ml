module Stubs = Bindings.Stubs(Bindings_stubs)

let time =
  Foreign.foreign
    "time"
    Ctypes.(ptr Posix_types.time_t @-> returning Posix_types.time_t)

let gmtime =
  Foreign.foreign
    "gmtime"
    Ctypes.(ptr Posix_types.time_t @-> returning (ptr Stubs.Tm.t))

let main () =
  let tme = Ctypes.allocate Posix_types.time_t (time Ctypes.(from_voidp Posix_types.time_t null)) in
  let tm = gmtime tme in
  Printf.printf "tm_hour = %d\n" Ctypes.(getf (!@ tm) Stubs.Tm.tm_hour);
  Printf.printf "tm_year = %d\n" Ctypes.(getf (!@ tm) Stubs.Tm.tm_year);
  Printf.printf "SHRT_MAX = %d\n" Stubs.Limits.shrt_max

let () = main ()
