(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Ctypes
open PosixTypes
open Foreign

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
let () = seal (tm : tm structure typ)

let time = foreign "time" (ptr time_t @-> returning_checking_errno time_t)
  
let asctime = foreign "asctime" (ptr tm @-> returning string)

let localtime = foreign "localtime" (ptr time_t @-> returning (ptr tm))

let () = begin
  let timep = allocate_n ~count:1 time_t in
  let time = time timep in
  assert (time = !@timep);
  let tm = localtime timep in
  Printf.printf "tm.tm_mon  = %d\n" (getf !@tm tm_mon);
  Printf.printf "tm.tm_year = %d\n" (getf !@tm tm_year);
  print_endline (asctime tm)
end
