(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Ctypes

type tm
let tm = structure "tm"
let (-:) ty label = field tm label ty
let tm_sec   = int -: "tm_sec"   (* seconds *)
let tm_min   = int -: "tm_min"   (* minutes *)
let tm_hour  = int -: "tm_hour"  (* hours *)
let tm_mday  = int -: "tm_mday"  (* day of the month *)
let tm_mon   = int -: "tm_mon"   (* month *)
let tm_year  = int -: "tm_year"  (* year *)
let tm_wday  = int -: "tm_wday"  (* day of the week *)
let tm_yday  = int -: "tm_yday"  (* day in the year *)
let tm_isdst = int -: "tm_isdst" (* daylight saving time *)
let () = seal (tm : tm structure typ)

let () = Cstubs.register_paths tm
  ~value:"Date_type.tm" ~typ:"Date_type.tm"
