(*
 * Copyright (c) 2016 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Stub generation driver for the Lwt jobs tests. *)

let cheader = "#include <unistd.h>"

let () = Tests_common.run ~cheader Sys.argv (module Functions.Stubs)
    ~concurrency:Cstubs.lwt_jobs
