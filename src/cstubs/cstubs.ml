(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Cstubs public interface. *)

let write_c = Cstubs_generate_c.fn
let write_ml = Cstubs_generate_ml.fn
let write_signature = Cstubs_generate_ml.signature
let register_paths = Cstubs_public_name.register_paths
