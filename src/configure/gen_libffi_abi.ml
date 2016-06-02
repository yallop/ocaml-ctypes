(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

let header ="\
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

let extra_headers = "#include <ffi.h>"

let write_line name symbol =
  try
    Printf.printf "let %s = Code %d\n" name (Extract_from_c.integer ~extra_headers symbol)
  with Not_found ->
    Printf.printf "let %s = Unsupported \"%s\"\n" name symbol

let () =
  begin
    print_string header;
    List.iter (fun (name, symbol) -> write_line name symbol) symbols
  end

