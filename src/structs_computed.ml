(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Static

let field (type k) (structured : (_, k) structured typ) label ftype =
  match structured with
  | Struct ({ spec = Incomplete spec } as s) ->
    let foffset = aligned_offset spec.isize (alignment ftype) in
    let field = { ftype; foffset; fname = label } in
    begin
      spec.isize <- foffset + sizeof ftype;
      s.fields <- BoxedField field :: s.fields;
      field
    end
  | Union ({ uspec = None } as u) ->
    let field = { ftype; foffset = 0; fname = label } in
    u.ufields <- BoxedField field :: u.ufields;
    field
  | Struct { tag; spec = Complete _ } -> raise (ModifyingSealedType tag)
  | Union { utag } -> raise (ModifyingSealedType utag)

let seal (type a) (type s) : (a, s) structured typ -> unit = function
  | Struct { fields = [] } -> raise (Unsupported "struct with no fields")
  | Struct { spec = Complete _; tag } -> raise (ModifyingSealedType tag)
  | Struct ({ spec = Incomplete { isize } } as s) 
      when all_passable s.fields ->
    s.fields <- List.rev s.fields;
    let bufspec = Static_stubs.allocate_bufferspec () in
    let () = 
      List.iter
        (fun (BoxedField {ftype; foffset}) ->
          let ArgType t = arg_type ftype in
          let offset = Static_stubs.add_argument bufspec t in
          assert (offset = foffset))
        s.fields
    in
    let salign = max_field_alignment s.fields in
    let ssize = aligned_offset isize salign in
    let sraw_io = Static_stubs.complete_struct_type bufspec in
    s.spec <- Complete { sraw_io; ssize; salign; spassable = true }
  | Struct ({ spec = Incomplete { isize } } as s) ->
    s.fields <- List.rev s.fields;
    let alignment = max_field_alignment s.fields in
    let size = aligned_offset isize alignment in
    let sraw_io = Static_stubs.make_unpassable_structspec ~size ~alignment in
    s.spec <- Complete { sraw_io; ssize = size; salign = alignment;
                         spassable = false }
  | Union { utag; uspec = Some _ } ->
    raise (ModifyingSealedType utag)
  | Union { ufields = [] } ->
    raise (Unsupported "union with no fields")
  | Union u -> begin
    u.ufields <- List.rev u.ufields;
    let usize = max_field_size u.ufields
    and ualignment = max_field_alignment u.ufields in
    u.uspec <- Some {
      usize = aligned_offset usize ualignment;
      ualignment = ualignment
    }
  end
