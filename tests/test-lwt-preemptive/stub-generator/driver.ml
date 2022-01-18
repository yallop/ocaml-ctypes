(*
 * Copyright (c) 2016 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Stub generation driver for the Lwt preemptive tests. *)

let cheader = "#include <math.h>
#include <sys/types.h>
#include <sys/stat.h>
#ifndef _MSC_VER
# include <unistd.h>
#endif
"

let () = Tests_common.run ~cheader Sys.argv (module Functions.Stubs)
    ~structs:(module Types.Struct_stubs)
    ~concurrency:Cstubs.lwt_preemptive
