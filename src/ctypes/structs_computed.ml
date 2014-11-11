(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Static

let aligned_offset offset alignment =
  match offset mod alignment with
    0 -> offset
  | overhang -> offset - overhang + alignment

let field structured fname ftype =
  match structured with
  | Struct { complete = true; tag } -> raise (ModifyingSealedType tag)
  | Struct spec ->
    let falign = alignment ftype in
    let foffset = aligned_offset spec.size falign in
    let field = { ftype; foffset; fname } in
    begin
      spec.size <- foffset + sizeof ftype;
      spec.align <- max falign spec.align;
      field
    end
  | _ -> raise (Unsupported "Adding a field to non-structured type")

let seal (type a) : a structure typ -> unit = function
  | Struct { size = 0 } -> raise (Unsupported "struct with no fields")
  | Struct { complete = true; tag } -> raise (ModifyingSealedType tag)
  | Struct spec ->
    begin
      spec.size <- aligned_offset spec.size spec.align;
      spec.complete <- true
    end
  | _ -> raise (Unsupported "Sealing a non-structured type")
