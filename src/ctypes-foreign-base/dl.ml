(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

type library

type flag = 
    RTLD_LAZY
  | RTLD_NOW
  | RTLD_GLOBAL
  | RTLD_NODELETE
  | RTLD_NOLOAD
  | RTLD_DEEPBIND

exception DL_error of string

(* void *dlopen(const char *filename, int flag); *)
external _dlopen : ?filename:string -> flags:int -> library option
  = "ctypes_dlopen"
    
(* void *dlsym(void *handle, const char *symbol); *)
external _dlsym : ?handle:library -> symbol:string -> int64 option
  = "ctypes_dlsym"

(* int dlclose(void *handle); *)
external _dlclose : handle:library -> int
  = "ctypes_dlclose"

(* char *dlerror(void); *)
external _dlerror : unit -> string option
  = "ctypes_dlerror"

external resolve_flag : flag -> int
  = "ctypes_resolve_dl_flag"

let _report_dl_error () =
  match _dlerror () with
    | None       -> failwith "dl_error: expected error, but no error reported"
    | Some error -> raise (DL_error (error))

let crush_flags f : 'a list -> int = List.fold_left (fun i o -> i lor (f o)) 0

let dlopen ?filename ~flags =
  match _dlopen ?filename ~flags:(crush_flags resolve_flag flags) with
    | Some library -> library
    | None         -> _report_dl_error ()

let dlclose ~handle =
  match _dlclose ~handle with
    | 0 -> ()
    | _ -> _report_dl_error ()

let dlsym ?handle ~symbol =
  match _dlsym ?handle ~symbol with
    | Some symbol -> Ctypes_raw.PtrType.of_int64 symbol
    | None        -> _report_dl_error ()
