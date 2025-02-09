[@@@warning "-9"]

module C = Configurator.V1

let header ="\
(*
 * Copyright (c) 2016 whitequark
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(** Size and alignment of C primitives

    This module defines the size and alignment of C primitives. By default, the
    values are generated at compile time for the host architecture and compiler.

    When the run-time architecture does not match the host architecture, a C
    data model can be selected at compile-time or run-time using the environment
    variable `CTYPES_DATA_MODEL`. Currently, only `CTYPES_DATA_MODEL=ILP32` is
    supported, which can be used to execute programs in 32-bit WebAssembly.
 *)

open Ctypes_primitive_types

module type CTYPES_PRIMITIVES = sig
  val sizeof : 'a prim -> int
  val alignment : 'a prim -> int
  val pointer_size : int
  val pointer_alignment : int
end

"

let other_primitives = {|
module Primitives_ILP32 = struct
let char_size = 1
let short_size = 2
let int_size = 4
let long_size = 4
let llong_size = 8
let size_t_size = 4
let pointer_size = 4
let pointer_alignment = 4
let sizeof : type a. a prim -> int = function
  | Char -> char_size
  | Schar -> char_size
  | Uchar -> char_size
  | Bool -> 1
  | Short -> short_size
  | Int -> int_size
  | Long -> long_size
  | Llong -> llong_size
  | Ushort -> short_size
  | Sint -> int_size
  | Uint -> int_size
  | Ulong -> long_size
  | Ullong -> llong_size
  | Size_t -> size_t_size
  | Int8_t -> 1
  | Int16_t -> 2
  | Int32_t -> 4
  | Int64_t -> 8
  | Uint8_t -> 1
  | Uint16_t -> 2
  | Uint32_t -> 4
  | Uint64_t -> 8
  | Float -> 4
  | Double -> 8
  | LDouble -> 16
  | Complex32 -> 8
  | Complex64 -> 16
  | Complexld -> 32
  | Nativeint -> 4
  | Camlint -> 4
let alignment : type a. a prim -> int = function
  | Char -> 1
  | Schar -> 1
  | Uchar -> 1
  | Bool -> 1
  | Short -> 2
  | Int -> 4
  | Long -> 4
  | Llong -> 8
  | Ushort -> 2
  | Sint -> 4
  | Uint -> 4
  | Ulong -> 4
  | Ullong -> 8
  | Size_t -> 4
  | Int8_t -> 1
  | Int16_t -> 2
  | Int32_t -> 4
  | Int64_t -> 8
  | Uint8_t -> 1
  | Uint16_t -> 2
  | Uint32_t -> 4
  | Uint64_t -> 8
  | Float -> 4
  | Double -> 8
  | LDouble -> 16
  | Complex32 -> 4
  | Complex64 -> 8
  | Complexld -> 16
  | Nativeint -> 4
  | Camlint -> 4
end
|}

let select_primitives =
  let default_primitives =
    match Sys.getenv_opt "CTYPES_DATA_MODEL" with
    | Some "ILP32" -> "Primitives_ILP32"
    | Some s -> failwith ("unexpected value for CTYPE_DATA_MODEL: " ^ s)
    | None -> "Primitives_host" in
  Format.sprintf {|
include
  (val
    (match Sys.getenv_opt "CTYPES_DATA_MODEL" with
     | Some "ILP32" -> (module Primitives_ILP32 : CTYPES_PRIMITIVES)
     | Some s -> failwith ("unexpected value for CTYPE_DATA_MODEL: " ^ s)
     | None -> (module %s : CTYPES_PRIMITIVES)))
 |}
    default_primitives

type c_format =
| No_format
| Known_format of string
| Defined_format of string

type c_primitive = {
  constructor : string;
  typ         : string;
  format      : c_format;
  size        : string;
  alignment   : string;
}

let c_primitive constructor typ format =
  { constructor; typ; format;
    size = "sizeof("^typ^")";
    alignment = "alignof("^typ^")"; }

let c_primitives = [
  c_primitive "Char"      "char"                (Known_format "d");
  c_primitive "Schar"     "signed char"         (Known_format "d");
  c_primitive "Uchar"     "unsigned char"       (Known_format "d");
  c_primitive "Bool"      "bool"                (Known_format "d");
  c_primitive "Short"     "short"               (Known_format "hd");
  c_primitive "Int"       "int"                 (Known_format "d");
  c_primitive "Long"      "long"                (Known_format "ld");
  c_primitive "Llong"     "long long"           (Known_format "lld");
  c_primitive "Ushort"    "unsigned short"      (Known_format "hu");
  c_primitive "Sint"      "int"                 (Known_format "d");
  c_primitive "Uint"      "unsigned int"        (Known_format "u");
  c_primitive "Ulong"     "unsigned long"       (Known_format "lu");
  c_primitive "Ullong"    "unsigned long long"  (Known_format "llu");
  c_primitive "Size_t"    "size_t"              (Known_format "zu");
  c_primitive "Int8_t"    "int8_t"              (Defined_format "PRId8");
  c_primitive "Int16_t"   "int16_t"             (Defined_format "PRId16");
  c_primitive "Int32_t"   "int32_t"             (Defined_format "PRId32");
  c_primitive "Int64_t"   "int64_t"             (Defined_format "PRId64");
  c_primitive "Uint8_t"   "uint8_t"             (Defined_format "PRIu8");
  c_primitive "Uint16_t"  "uint16_t"            (Defined_format "PRIu16");
  c_primitive "Uint32_t"  "uint32_t"            (Defined_format "PRIu32");
  c_primitive "Uint64_t"  "uint64_t"            (Defined_format "PRIu64");
  c_primitive "Float"     "float"               (Known_format ".12g");
  c_primitive "Double"    "double"              (Known_format ".12g");
  c_primitive "LDouble"   "long double"         (Known_format ".12Lg");
  c_primitive "Complex32" "float _Complex"      (No_format);
  c_primitive "Complex64" "double _Complex"     (No_format);
  c_primitive "Complexld" "long double _Complex"(No_format);
  c_primitive "Nativeint" "intnat"              (Defined_format "REAL_ARCH_INTNAT_PRINTF_FORMAT \"d\"");
  { constructor = "Camlint";
    typ         = "intnat";
    format      = Defined_format "REAL_ARCH_INTNAT_PRINTF_FORMAT \"d\"";
    size        = "sizeof(intnat)";
    alignment   = "alignof(intnat)" };
]

open Printf

let generate name typ f =
  printf "let %s : type a. a prim -> %s = function\n" name typ;
  List.iter (fun c_primitive ->
    printf "  | %s -> " c_primitive.constructor;
    begin try
      f c_primitive
    with Not_found ->
      failwith (name^": "^c_primitive.constructor)
    end;
    printf "\n") c_primitives

let prelude = "\
#if defined(__MINGW32__) || defined(__MINGW64__)
#define __USE_MINGW_ANSI_STDIO 1
#include <stdio.h> /* see: https://sourceforge.net/p/mingw-w64/bugs/627/ */
#endif

#include <stdint.h>
#include <stdbool.h>
#include <inttypes.h>
#include <caml/mlvalues.h>

#define alignof(T) (offsetof(struct { char c; T t; }, t))
#define STRINGIFY1(x) #x
#define STRINGIFY(x) STRINGIFY1(x)

#if __USE_MINGW_ANSI_STDIO && defined(__MINGW64__)
#define REAL_ARCH_INTNAT_PRINTF_FORMAT \"ll\"
#else
#define REAL_ARCH_INTNAT_PRINTF_FORMAT ARCH_INTNAT_PRINTF_FORMAT
#endif
"
let includes = []

let () =
  C.main ~name:"ctypes" (fun c ->
    let import_int l =
      match C.C_define.(import c ~prelude ~includes [l,Type.Int]) with
      |[_,C.C_define.Value.Int i] -> i
      |_ -> failwith ("unable to find integer definition for " ^ l) in
    let import_string l  =
      match C.C_define.(import c ~prelude ~includes [l,Type.String]) with
      |[_,C.C_define.Value.String s] -> s
      |_ -> failwith ("unable to find string definition for " ^ l) in
    print_string header;
    print_endline "module Primitives_host = struct";
    generate "sizeof" "int" (fun { size } ->
      printf "%d" (import_int size));
    generate "alignment" "int" (fun { alignment } ->
      printf "%d" (import_int alignment));
    printf "let pointer_size = %d\n" (import_int "sizeof(void*)");
    printf "let pointer_alignment = %d\n" (import_int "alignof(void*)");
    print_endline "end";
    print_endline other_primitives;
    print_endline select_primitives;
    generate "name" "string" (fun { typ } ->
      printf "%S" (import_string ("STRINGIFY("^typ^")")));
    generate "format_string" "string option" (fun { format } ->
      match format with
      | Known_format str ->
        printf "Some %S" ("%"^str)
      | Defined_format str ->
        printf "Some %S" ("%"^(import_string str))
      | No_format ->
        printf "None");
  )
