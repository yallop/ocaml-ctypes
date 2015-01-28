(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Program discover
 * Copyright (C) 2012 Anil Madhavapeddy
 * Copyright (C) 2010 Jérémie Dimino
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, with linking exceptions;
 * either version 2.1 of the License, or (at your option) any later
 * version. See COPYING file for details.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 *)

(* Discover available features *)

open Printf

(* +-----------------------------------------------------------------+
   | Search path                                                     |
   +-----------------------------------------------------------------+ *)

(* List of search paths for header files, mostly for MacOS
   users. libffi is installed by port systems into non-standard
   locations by default on MacOS.

   We use a hardcorded list of path + the ones from C_INCLUDE_PATH and
   LIBRARY_PATH.
*)

let ( // ) = Filename.concat

let default_search_paths =
  List.map (fun dir -> (dir ^ "/include", dir ^ "/lib")) [
    "/usr";
    "/usr/local";
    "/opt";
    "/opt/local";
    "/sw";
    "/mingw";
  ]

let is_win = Sys.os_type = "Win32"

let path_sep = if is_win then ';' else ':'

let split_path str =
  let len = String.length str in
  let rec aux i =
    if i >= len then
      []
    else
      let j = try String.index_from str i path_sep with Not_found -> len in
      String.sub str i (j - i) :: aux (j + 1)
  in
  aux 0

let search_paths =
  let get var f =
    try
      List.map f (split_path (Sys.getenv var))
    with Not_found ->
      []
  in
  List.flatten [
    get "C_INCLUDE_PATH" (fun dir -> (dir, dir // ".." // "lib"));
    get "LIBRARY_PATH" (fun dir -> (dir // ".." // "include", dir));
    default_search_paths;
  ]

(* +-----------------------------------------------------------------+
   | Test codes                                                      |
   +-----------------------------------------------------------------+ *)

let caml_code = "
external test : unit -> unit = \"ffi_test\"
let () = test ()
"

let libffi_code = "
#include <caml/mlvalues.h>
#include <ffi.h>

CAMLprim value ffi_test()
{
  ffi_prep_closure(NULL, NULL, NULL, NULL);
  return Val_unit;
}
"

(* +-----------------------------------------------------------------+
   | Compilation                                                     |
   +-----------------------------------------------------------------+ *)

let ocamlc = ref "ocamlc"
let ext_obj = ref ".o"
let exec_name = ref "a.out"
let os_type = ref "Unix"
let ccomp_type = ref "cc"
let ffi_dir = ref ""
let is_homebrew = ref false
let homebrew_prefix = ref "/usr/local"

let log_file = ref ""
let caml_file = ref ""

(* Search for a header file in standard directories. *)
let search_header header =
  let rec loop = function
    | [] ->
        None
    | (dir_include, dir_lib) :: dirs ->
        if Sys.file_exists (dir_include // header) then
          Some (dir_include, dir_lib)
        else
          loop dirs
  in
  loop search_paths

let compile (opt, lib) stub_file =
  ksprintf
    Sys.command
    "%s -custom %s %s %s %s > %s 2>&1"
    !ocamlc
    (String.concat " " (List.map (sprintf "-ccopt %s") opt))
    (Filename.quote stub_file)
    (Filename.quote !caml_file)
    (String.concat " " (List.map (sprintf "-cclib %s") lib))
    (Filename.quote !log_file)
  = 0

let safe_remove file_name =
  try
    Sys.remove file_name
  with exn ->
    ()

let test_code args stub_code =
  let stub_file, oc = Filename.open_temp_file "ffi_stub" ".c" in
  let cleanup () =
    safe_remove stub_file;
    safe_remove (Filename.chop_extension (Filename.basename stub_file) ^ !ext_obj)
  in
  try
    output_string oc stub_code;
    flush oc;
    close_out oc;
    let result = compile args stub_file in
    cleanup ();
    result
  with exn ->
    (try close_out oc with _ -> ());
    cleanup ();
    raise exn

let test_feature name test =
  begin
    fprintf stderr "testing for %s:%!" name;
    if test () then begin
      fprintf stderr " %s available\n%!" (String.make (34 - String.length name) '.');
      true
    end else begin
      fprintf stderr " %s unavailable\n%!" (String.make (34 - String.length name) '.');
      false
    end
  end

(* +-----------------------------------------------------------------+
   | pkg-config                                                      |
   +-----------------------------------------------------------------+ *)

let split = Str.(split (regexp " +"))

let brew_libffi_version flags =
  if ksprintf Sys.command "brew ls libffi --versions | awk '{print $NF}' > %s 2>&1" !log_file = 0 then begin
    let ic = open_in !log_file in
    let line = input_line ic in
    close_in ic;
    if line = "" then begin
      print_endline "You need to 'brew install libffi' to get a suitably up-to-date version";
      exit 1
    end;
    line
  end else
    raise Exit

let pkg_config choose flags =
  let cmd () =
    match choose with 
    |`Default -> ksprintf Sys.command "pkg-config %s > %s 2>&1" flags !log_file
    |`Homebrew ver -> ksprintf Sys.command "env PKG_CONFIG_PATH=%s/Cellar/libffi/%s/lib/pkgconfig %s/bin/pkg-config %s > %s 2>&1" !homebrew_prefix ver !homebrew_prefix flags !log_file
  in
  if cmd () = 0 then begin
    let ic = open_in !log_file in
    let line = input_line ic in
    close_in ic;
    split line
  end else
    raise Exit

let pkg_config_flags name =
  let pkg_config =
    if !is_homebrew then
      pkg_config (`Homebrew (brew_libffi_version ()))
    else
      pkg_config `Default
  in
  try
    (* Get compile flags. *)
    let opt = ksprintf pkg_config "--cflags %s" name in
    (* Get linking flags. *)
    let lib =
      if !ccomp_type = "msvc" then
        ksprintf pkg_config "--libs-only-L %s" name @ ksprintf pkg_config "--libs-only-l --msvc-syntax %s" name
      else
        ksprintf pkg_config "--libs %s" name
    in
    Some (opt, lib)
  with Exit ->
    None

let lib_flags env_var_prefix fallback =
  let get var = try Some (split (Sys.getenv var)) with Not_found -> None in
  match get (env_var_prefix ^ "_CFLAGS"), get (env_var_prefix ^ "_LIBS") with
    | Some opt, Some lib ->
        (opt, lib)
    | x ->
        let opt, lib = fallback () in
        match x with
          | Some opt, Some lib ->
              assert false
          | Some opt, None ->
              (opt, lib)
          | None, Some lib ->
              (opt, lib)
          | None, None ->
              (opt, lib)

(* +-----------------------------------------------------------------+
   | Entry point                                                     |
   +-----------------------------------------------------------------+ *)

let () =
  let args = [
    "-ocamlc", Arg.Set_string ocamlc, "<path> ocamlc";
    "-ext-obj", Arg.Set_string ext_obj, "<ext> C object files extension";
    "-exec-name", Arg.Set_string exec_name, "<name> name of the executable produced by ocamlc";
    "-ccomp-type", Arg.Set_string ccomp_type, "<ccomp-type> C compiler type";
  ] in
  Arg.parse args ignore "check for external C libraries and available features\noptions are:";

  (* Put the caml code into a temporary file. *)
  let file, oc = Filename.open_temp_file "ffi_caml" ".ml" in
  caml_file := file;
  output_string oc caml_code;
  close_out oc;

  log_file := Filename.temp_file "ffi_output" ".log";

  (* Cleanup things on exit. *)
  at_exit (fun () ->
             safe_remove !log_file;
             safe_remove !exec_name;
             safe_remove !caml_file;
             safe_remove (Filename.chop_extension !caml_file ^ ".cmi");
             safe_remove (Filename.chop_extension !caml_file ^ ".cmo"));

  let setup_data = ref [] in

  (* Test for MacOS X Homebrew. *)
  is_homebrew :=
    test_feature "brew"
      (fun () ->
         ksprintf Sys.command "brew info libffi > %s 2>&1" !log_file = 0);

  let get_homebrew_prefix () =
    let cmd () = ksprintf Sys.command "brew --prefix > %s" !log_file in
    if cmd () = 0 then begin
      let ic = open_in !log_file in
      let line = input_line ic in
      close_in ic;
      line
    end else
      raise Exit
  in

  (* Test for pkg-config. If we are on MacOS X, we need the latest pkg-config
   * from Homebrew *)
  let have_pkg_config =
    (match !is_homebrew with
     | true -> (* Look in `brew for the right pkg-config *)
       homebrew_prefix := get_homebrew_prefix ();
       test_feature "pkg-config"
         (fun () ->
            ksprintf Sys.command "%s/bin/pkg-config --version > %s 2>&1" !homebrew_prefix !log_file = 0);
     | false ->
       test_feature "pkg-config"
         (fun () ->
            ksprintf Sys.command "pkg-config --version > %s 2>&1" !log_file = 0);
    )
  in
  if not have_pkg_config then
    fprintf stderr "Warning: the 'pkg-config' command is not available."
  ;

  let test_libffi () =
    let opt, lib =
      lib_flags "LIBFFI"
        (fun () ->
          match if have_pkg_config then pkg_config_flags "libffi" else None with
            | Some (opt, lib) ->
                (opt, lib)
            | None ->
                match search_header "ffi.h" with
                  | Some (dir_i, dir_l) ->
                      (["-I" ^ dir_i], ["-L" ^ dir_l; "-lffi"])
                  | None ->
                      ([], ["-lffi"]))
    in
    setup_data := ("libffi_opt", opt) :: ("libffi_lib", lib) :: !setup_data;
    test_code (opt, lib) libffi_code
  in

  if not (test_feature "libffi" test_libffi) then begin
    fprintf stderr "
The following required C libraries are missing: libffi.
Please install them and retry. If they are installed in a non-standard location
or need special flags, set the environment variables <LIB>_CFLAGS and <LIB>_LIBS
accordingly and retry.

For example, if libffi is installed in /opt/local, you can type:

export LIBFFI_CFLAGS=-I/opt/local/include
export LIBFFI_LIBS=-L/opt/local/lib

" ;
    exit 1
  end;

  (* Add flags to setup.data *)
  let setup_data_lines =
    List.fold_left
      (fun lines (name, args) ->
         sprintf "%s=%s" name (String.concat " " args) :: lines)
      [] !setup_data
  in
  List.iter (Printf.printf "%s\n") setup_data_lines

