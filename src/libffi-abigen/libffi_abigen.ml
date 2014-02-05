(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

let header ="
(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Support for various ABIs *)

type abi = Code of int | Unsupported of string

let abi_code = function
   Code c -> c
 | Unsupported sym -> raise (Ctypes.Unsupported sym)

"

let symbols = [
  ("aix"              , "FFI_AIX");
  ("darwin"           , "FFI_DARWIN");
  ("eabi"             , "FFI_EABI");
  ("fastcall"         , "FFI_FASTCALL");
  ("gcc_sysv"         , "FFI_GCC_SYSV");
  ("linux"            , "FFI_LINUX");
  ("linux64"          , "FFI_LINUX64");
  ("linux_soft_float" , "FFI_LINUX_SOFT_FLOAT");
  ("ms_cdecl"         , "FFI_MS_CDECL");
  ("n32"              , "FFI_N32");
  ("n32_soft_float"   , "FFI_N32_SOFT_FLOAT");
  ("n64"              , "FFI_N64");
  ("n64_soft_float"   , "FFI_N64_SOFT_FLOAT");
  ("o32"              , "FFI_O32");
  ("o32_soft_float"   , "FFI_O32_SOFT_FLOAT");
  ("osf"              , "FFI_OSF");
  ("pa32"             , "FFI_PA32");
  ("stdcall"          , "FFI_STDCALL");
  ("sysv"             , "FFI_SYSV");
  ("thiscall"         , "FFI_THISCALL");
  ("unix"             , "FFI_UNIX");
  ("unix64"           , "FFI_UNIX64");
  ("v8"               , "FFI_V8");
  ("v8plus"           , "FFI_V8PLUS");
  ("v9"               , "FFI_V9");
  ("vfp"              , "FFI_VFP");
  ("default_abi"      , "FFI_DEFAULT_ABI");
]

let getenv ~default name =
  try Sys.getenv name
  with Not_found -> default

let read_output_int input_filename output_filename =
  let cmd = 
    Printf.sprintf "%s -o %s %s %s 2>/dev/null && %s"
      (getenv ~default:"cc" "CC")
      output_filename
      (getenv ~default:"" "CFLAGS")
      input_filename
      output_filename
  in
  let inch = Unix.open_process_in cmd in
  try Some (Scanf.fscanf inch "%d" (fun i -> i))
  with End_of_file -> None

let generate_program symbol =
  Printf.sprintf "\
#include <ffi.h>
#include <stdio.h>

int main(void)
{
  printf(\"%%d\\n\", %s);
  return 0;
}
" symbol

let determine_code symbol =
  let program = generate_program symbol in
  let input_file = Filename.temp_file "ctypes_libffi_config" ".c"
  and output_file = Filename.temp_file "ctypes_libffi_config" "" in
  let outch = open_out input_file in
  begin
    Printf.fprintf outch "%s" program;
    flush outch;
    close_out outch;
  end;
  let result = read_output_int input_file output_file in
  begin
    Sys.remove input_file;
    Sys.remove output_file;
    result
  end

let write_line name symbol =
  match determine_code symbol with
  | None -> Printf.printf "let %s = Unsupported \"%s\"\n" name symbol
  | Some code -> Printf.printf "let %s = Code %d\n" name code

let () =
  begin
    print_string header;
    List.iter (fun (name, symbol) -> write_line name symbol) symbols
  end

