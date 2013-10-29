(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(** Bindings to the dlopen / dlsym interface. *)

type library
(** The type of dynamic libraries, as returned by {!dlopen}. *)

exception DL_error of string
(** An error condition occurred when calling {!dlopen}, {!dlclose} or
    {!dlsym}.  The argument is the string returned by the [dlerror]
    function. *)

(** Flags for {!dlopen} *)
type flag = 
    RTLD_LAZY
  | RTLD_NOW
  | RTLD_GLOBAL
  | RTLD_NODELETE
  | RTLD_NOLOAD
  | RTLD_DEEPBIND

val dlopen : ?filename:string -> flags:flag list -> library
(** Open a dynamic library. *)

val dlclose : handle:library -> unit
(** Close a dynamic library. *)

val dlsym : ?handle:library -> symbol:string -> Ctypes_raw.voidp
(** Look up a symbol in a dynamic library. *)
