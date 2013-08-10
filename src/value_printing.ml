(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Static
open Dynamic

let rec format : type a. a typ -> Format.formatter -> a -> unit
  = fun typ fmt v -> match typ with
    Void -> Format.pp_print_string fmt (Dynamic_stubs.string_of RawTypes.void v)
  | Primitive p -> Format.pp_print_string fmt (Dynamic_stubs.string_of p v)
  | Pointer _ -> format_ptr fmt v
  | Struct _ -> format_struct fmt v
  | Union _ -> format_union fmt v
  | Array (a, n) -> format_array fmt v
  | Abstract abs -> Format.pp_print_string fmt "<abstract>"
    (* For now, just print the underlying value in a view *)
  | View {write; ty} -> format ty fmt (write v)
  | FunctionPointer (name, fn) -> Format.pp_print_string fmt "<fun>"
and format_struct : type a. Format.formatter -> a structure -> unit
  = fun fmt ({structured = {reftype = Struct {fields}}} as s) ->
    let open Format in
    fprintf fmt "{@;<1 2>@[";
    format_fields "," fields fmt s;
    fprintf fmt "@]@;<1 0>}"
and format_union : type a. Format.formatter -> a union -> unit
  = fun fmt ({structured = {reftype = Union {ufields}}} as u) ->
    let open Format in
    fprintf fmt "{@;<1 2>@[";
    format_fields " |" ufields fmt u;
    fprintf fmt "@]@;<1 0>}"
and format_array : type a. Format.formatter -> a array -> unit
  = fun fmt ({astart = {reftype}; alength} as arr) ->
    let open Format in
    fprintf fmt "{@;<1 2>@[";
    for i = 0 to alength - 1 do
      format reftype fmt (Array.get arr i);
      if i <> alength - 1 then
        fprintf fmt ",@;"
    done;
    fprintf fmt "@]@;<1 0>}"
and format_fields : type a b. string -> (a, b) structured boxed_field list ->
                              Format.formatter -> (a, b) structured -> unit
  = fun sep fields fmt s ->
    let last_field = List.length fields - 1 in
    let open Format in
    List.iteri
      (fun i (BoxedField ({ftype; foffset} as f)) ->
        fprintf fmt "@[%a@]%s@;" (format ftype) (getf s f) 
          (if i <> last_field then sep else ""))
      (List.rev fields)
and format_ptr : type a. Format.formatter -> a ptr -> unit
  = fun fmt {raw_ptr; reftype; pbyte_offset} ->
    Format.fprintf fmt "%s"
      (Dynamic_stubs.string_of
         RawTypes.pointer
         (RawTypes.PtrType.(add raw_ptr (of_int pbyte_offset))))

let string_of typ v = Common.string_of (format typ) v
