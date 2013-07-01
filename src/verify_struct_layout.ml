open Printf

(* Obtain basic struct layout information.

   No attempt to verify field types for the moment.
*)

type field_info = {
  label: string;
  offset: int;
  size : int;
}

type struct_info = {
  tag : string;
  fields : field_info list;
  size: int
}

exception UnknownStruct of string
exception UnknownField of string
exception CompilationError of exn

let _integer_value ~headers expr : int =
  (* Proof-of-concept implementation. *)
  let prologue = String.concat "\n"
    (List.map (Printf.sprintf "#include \"%s\"") headers)
  in
  let id = Str.(global_replace (regexp "\\.") "_"
                  (sprintf "x_%f" (Unix.gettimeofday ()))) in
  let ouch = open_out (sprintf "/tmp/%s.c" id) in
  begin
    fprintf ouch "#include <stddef.h>\n%s\n\n int %s = (%s);" prologue id expr;
    close_out ouch;
    ignore (Sys.command (sprintf "gcc -shared -fPIC -o /tmp/%s.so /tmp/%s.c" id id));
    let lib = Dl.(dlopen ~filename:(sprintf "/tmp/%s.so" id) ~flags:[RTLD_NOW]) in
    Ctypes.(!@ (Foreign.foreign_value ~from:lib id int))
  end

let integer_value ~headers expr =
  try _integer_value ~headers expr
  with e -> raise (CompilationError e)

let struct_size ~headers tag = 
  try integer_value ~headers (sprintf "sizeof (struct %s)" tag)
  with CompilationError _ -> raise (UnknownStruct tag)

let field_size ~headers ~tag label =
  integer_value ~headers (sprintf "sizeof (((struct %s *)NULL)->%s)" tag label)

let field_offset ~headers ~tag label =
  integer_value ~headers (sprintf "offsetof (struct %s, %s)" tag label)

let field_info ~headers ~tag label =
  try { label ;
        offset = field_offset ~headers ~tag label ;
        size = field_size ~headers ~tag label }
  with CompilationError _ -> raise (UnknownField label)
    
let struct_info ?(headers=[]) ~tag ~fields : struct_info =
  let size = struct_size ~headers tag in
  let fields = List.sort (fun {offset=l} {offset=r} -> compare l r)
    (List.map (field_info ~headers ~tag) fields) in
  { tag; size; fields }
