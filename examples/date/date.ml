open Ffi.C
open Type
open PosixTypes
open Struct

type tm
let tm = structure "tm"
let tm_sec   = tm *:* int (* seconds *)
let tm_min   = tm *:* int (* minutes *)
let tm_hour  = tm *:* int (* hours *)
let tm_mday  = tm *:* int (* day of the month *)
let tm_mon   = tm *:* int (* month *)
let tm_year  = tm *:* int (* year *)
let tm_wday  = tm *:* int (* day of the week *)
let tm_yday  = tm *:* int (* day in the year *)
let tm_isdst = tm *:* int (* daylight saving time *)
let () = seals (tm : tm structure typ)

let time = foreign "time" (ptr time_t @-> syscall time_t)
  
let asctime = foreign "asctime" (ptr tm @-> returning (ptr char))

let localtime = foreign "localtime" (ptr time_t @-> returning (ptr tm))

let () = begin
  let timep = Ptr.allocate ~count:1 time_t in
  let time = time timep in
  assert (time = Ptr.(!timep));
  let tm = localtime timep in
  print_endline (string_of_char_ptr (asctime tm))
end
