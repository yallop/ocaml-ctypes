(*
 * Copyright (c) 2016 whitequark.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

let getenv ~default name =
  try Sys.getenv name
  with Not_found -> default

let nsplit sep str = Str.(split (regexp_string sep)) str

let read_output program =
  let input_filename = Filename.temp_file "ctypes_libffi_config" ".c" in
  let channel = open_out input_filename in
  output_string channel program;
  close_out channel;
  let output_filename = (Filename.chop_suffix input_filename ".c") ^ ".o" in
  let cwd = Sys.getcwd () in
  let cmd =
    Printf.sprintf "%s ocamlc -verbose %s %s -c 1>&2"
      (getenv ~default:"ocamlfind" "OCAMLFIND")
      ((getenv ~default:"" "CFLAGS") |>
       (nsplit " ") |>
       (List.map (fun s -> "-ccopt " ^ Filename.quote s)) |>
       (String.concat " "))
      (Filename.quote input_filename)
  in
  prerr_endline cmd;
  Sys.chdir (Filename.dirname input_filename);
  ignore (Sys.command cmd);
  Sys.chdir cwd;
  Sys.remove input_filename;
  if not (Sys.file_exists output_filename) then
    raise Not_found;
  let channel = open_in_bin output_filename in
  let length = in_channel_length channel in
  let result = Bytes.create length in
  really_input channel result 0 length;
  close_in channel;
  Sys.remove output_filename;
  Bytes.to_string result

let find_from haystack pos needle = Str.(search_forward (regexp_string needle) haystack pos)

let prefix = "BEGIN-"
let suffix = "-END"
let extract s =
  let begin_pos = find_from s 0 prefix + String.length prefix in
  let end_pos   = find_from s 0 suffix in
  String.sub s begin_pos (end_pos - begin_pos)

let headers = "\
#if defined(__MINGW32__) || defined(__MINGW64__)
#define __USE_MINGW_ANSI_STDIO 1
#include <stdio.h> /* see: https://sourceforge.net/p/mingw-w64/bugs/627/ */
#endif
#include <stdint.h>
#include <stdbool.h>
#include <inttypes.h>
#include <caml/mlvalues.h>
"

let integer ?(extra_headers="") expression =
  let code = Printf.sprintf "%s
%s

#define alignof(T) (offsetof(struct { char c; T t; }, t))

#define D0(x) ('0'+(x/1         )%%10)
#define D1(x) ('0'+(x/10        )%%10), D0(x)
#define D2(x) ('0'+(x/100       )%%10), D1(x)
#define D3(x) ('0'+(x/1000      )%%10), D2(x)
#define D4(x) ('0'+(x/10000     )%%10), D3(x)
#define D5(x) ('0'+(x/100000    )%%10), D4(x)
#define D6(x) ('0'+(x/1000000   )%%10), D5(x)
#define D7(x) ('0'+(x/10000000  )%%10), D6(x)
#define D8(x) ('0'+(x/100000000 )%%10), D7(x)
#define D9(x) ('0'+(x/1000000000)%%10), D8(x)
const char s[] = {
  'B', 'E', 'G', 'I', 'N', '-',
  D9((%s)),
  '-', 'E', 'N', 'D'
};
" headers extra_headers expression in
  int_of_string (extract (read_output code))

let string ?(extra_headers="") expression =
  let code = Printf.sprintf "%s
%s

#define STRINGIFY1(x) #x
#define STRINGIFY(x) STRINGIFY1(x)

#if __USE_MINGW_ANSI_STDIO && defined(__MINGW64__)
#define REAL_ARCH_INTNAT_PRINTF_FORMAT \"ll\"
#else
#define REAL_ARCH_INTNAT_PRINTF_FORMAT ARCH_INTNAT_PRINTF_FORMAT
#endif

const char *s = \"BEGIN-\" %s \"-END\";
" headers extra_headers expression in
  extract (read_output code)
