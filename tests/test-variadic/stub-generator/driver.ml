(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Stub generation driver for the variadic function tests. *)

let cheader = "
#include <stdio.h>
#define snprintf1 snprintf
#define snprintf2 snprintf
#define snprintf3 snprintf
#define snprintf4 snprintf
"

let () = Tests_common.run ~cheader Sys.argv (module Functions.Stubs)
