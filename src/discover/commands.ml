(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

let unwind_protect ~cleanup f x =
  let rv = try f x
    with exn ->
      begin
        cleanup x;
        raise exn
      end
  in
  begin
    cleanup x;
    rv
  end

let with_open_output_file ~filename f =
  unwind_protect ~cleanup:close_out f (open_out filename)

let with_open_input_file ~filename f =
  unwind_protect ~cleanup:close_in f (open_in filename)

let file_contents ~filename : string =
  Bytes.to_string
    (with_open_input_file ~filename
      (fun file ->
         let () = set_binary_mode_in file true in
         let size = in_channel_length file in
         let buf = Bytes.create size in
         let () = really_input file buf 0 size in
         buf))

type command_output = {
  status: int;
  stdout: string;
  stderr: string;
}

let temp_dir = "."

(* The use of [unixify] below is not ideal: it's a workaround for the Windows
   build, which uses a mixture of Windows- and Unix-style paths due to using MinGW
   to compile OCaml and Bash for the shell.
*)
let unixify = Str.(global_replace (regexp "\\\\") "/")

let shell_command_results command =
  let stdout_filename = Filename.temp_file ~temp_dir "ctypes_config" ".stdout" in
  let stderr_filename = Filename.temp_file ~temp_dir "ctypes_config" ".stderr" in
  unwind_protect
    (fun () ->
       let full_command =
         Printf.sprintf "(%s) > %s 2> %s" command 
           (unixify stdout_filename)
           (unixify stderr_filename)
       in
       let status = Sys.command full_command in
       let stdout = file_contents stdout_filename in
       let stderr = file_contents stderr_filename in
       { status; stdout; stderr }
    ) ()
    ~cleanup:begin fun () ->
      Sys.remove stdout_filename;
      Sys.remove stderr_filename;
    end


let command fmt =
  Printf.ksprintf shell_command_results fmt

let command_output' command =
  (shell_command_results command).stdout

let command_succeeds' command = (shell_command_results command).status = 0

let command_succeeds fmt =
  Printf.ksprintf command_succeeds' fmt
