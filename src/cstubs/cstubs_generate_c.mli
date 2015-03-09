(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* C stub customization options. *)
type inverted_stubs_options =
  {
    (* will wrap [caml_release_runtime_system] and
       [caml_acquire_runtime_system] around callbacks.*)
    runtime_lock: bool;

    (* will wrap each function call with [caml_c_thread_register] and
       [caml_c_thread_unregister]. *)
    c_thread_register: bool;

    (* Arbitrary C code to be inserted before/after the
       function. Typically, can be used to insert mutex
       acquire/release. *)
    prelude: string option;
    epilogue: string option;
  }

(* C stub generation *)

val fn : cname:string -> stub_name:string ->
         Format.formatter -> 'a Ctypes.fn -> unit

val value : cname:string -> stub_name:string -> Format.formatter ->
         'a Ctypes.typ -> unit

val inverse_fn : stub_name:string -> options:inverted_stubs_options ->
  Format.formatter -> 'a Ctypes.fn -> unit

val inverse_fn_decl : stub_name:string -> Format.formatter ->
         'a Ctypes.fn -> unit
