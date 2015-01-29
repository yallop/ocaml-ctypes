(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

val unwind_protect : cleanup:('a -> 'c) -> ('a -> 'b) -> 'a -> 'b
val with_open_output_file : filename:string -> (out_channel -> 'a) -> 'a

val file_contents : filename:string -> string

type command_output = {
  status: int;
  stdout: string;
  stderr: string;
}

val command : ('a, unit, string, command_output) format4 -> 'a

val command_succeeds : ('a, unit, string, bool) format4 -> 'a
