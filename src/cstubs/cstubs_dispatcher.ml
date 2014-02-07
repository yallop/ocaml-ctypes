(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

let fprintf = Format.fprintf

let write_call fmt (name, nargs) =
  fprintf fmt "@[%s(@[" name;
  for i = 1 to nargs do
    fprintf fmt "frame->x%d%(%)" i
      (if i <> nargs then ",@ " else "")
  done;
  fprintf fmt ")@]@]"

let write_entry ~prefix fmt (name, arity) =
  let pr x = fprintf fmt x in
  let frame_name = Printf.sprintf "%s_%s_frame" prefix name
  and fn_name = Printf.sprintf "%s_%s_fn" prefix name in
  pr "@[case@ %s:@]@ @[<2>@ @[" fn_name;
  pr "@[<2>@[struct@ %s@ *frame@]@ @[=@ (struct@ %s@ *)call_buffer;@]@]@\n"
    frame_name frame_name;
  pr "frame->return_value = @[%a@];@\n" write_call (name, arity);
  pr "continue;@\n@]@]@\n"

let write_dispatcher ~prefix fmt entries =
  let pr x = fprintf fmt x in
  let base_frame = Printf.sprintf "%s_frame" prefix in
  pr "@[void@ %s_dispatch@[(struct@ %s@ *call_buffer,@ futex@ *futex)@]@]@\n"
    prefix base_frame;
  pr "{@[<2>@\n@[@[for@ (;;)@]@\n{@[<2>@\n";
  pr "@[<2>cstubs_context_switch(futex);@]@\n";
  pr "@[switch@ (call_buffer->fn_name)@]@\n{@[@\n";
  List.iter (write_entry ~prefix fmt) entries;
  pr "@]}@]@\n}@]@]\n}@."

let make_frame_struct name fn =
  let structure = Ctypes.structure name in
  (* This shouldn't really be an int.  We need enum support, or at least
     typedef support. *)
  let _ = Ctypes.field structure "fn_name" Ctypes.int in
  let rec loop : type a. int -> a Ctypes.fn -> unit =
    fun i -> function
    | Static.Function (typ, fn) ->
      let _ = Ctypes.field structure (Printf.sprintf "x%d" i) typ in
      loop (i + 1) fn
    | Static.Returns typ ->
      ignore (Ctypes.field structure "return_value" typ)
  in
  begin
    loop 1 fn;
    Ctypes.seal structure;
    structure
  end

let write_frame_struct name fmt fn =
  Ctypes.format_typ fmt (make_frame_struct name fn)
