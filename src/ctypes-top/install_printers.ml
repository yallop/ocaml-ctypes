(* Adapted from Anil Madhavapeddy's ocaml-uri package. *)

let printers = [ "Ctypes_top.Ctypes_printers.format_typ";
                 "Ctypes_top.Ctypes_printers.format_fn"; 
                 "Ctypes_top.Ctypes_printers.format_long";
                 "Ctypes_top.Ctypes_printers.format_llong";
                 "Ctypes_top.Ctypes_printers.format_uchar";
                 "Ctypes_top.Ctypes_printers.format_uint8";
                 "Ctypes_top.Ctypes_printers.format_uint16";
                 "Ctypes_top.Ctypes_printers.format_uint32";
                 "Ctypes_top.Ctypes_printers.format_uint64";
                 "Ctypes_top.Ctypes_printers.format_size_t";
                 "Ctypes_top.Ctypes_printers.format_ushort";
                 "Ctypes_top.Ctypes_printers.format_uint";
                 "Ctypes_top.Ctypes_printers.format_ulong";
                 "Ctypes_top.Ctypes_printers.format_ullong";
                 "Ctypes_top.Ctypes_printers.format_pointer";
                 "Ctypes_top.Ctypes_printers.format_struct";
                 "Ctypes_top.Ctypes_printers.format_union";
                 "Ctypes_top.Ctypes_printers.format_array";
                 "Ctypes_top.Ctypes_printers.format_blkcnt_t";
                 "Ctypes_top.Ctypes_printers.format_blksize_t";
                 "Ctypes_top.Ctypes_printers.format_clock_t";
                 "Ctypes_top.Ctypes_printers.format_dev_t";
                 "Ctypes_top.Ctypes_printers.format_fsblkcnt_t";
                 "Ctypes_top.Ctypes_printers.format_fsfilcnt_t";
                 "Ctypes_top.Ctypes_printers.format_gid_t";
                 "Ctypes_top.Ctypes_printers.format_id_t";
                 "Ctypes_top.Ctypes_printers.format_ino_t";
                 "Ctypes_top.Ctypes_printers.format_mode_t";
                 "Ctypes_top.Ctypes_printers.format_nlink_t";
                 "Ctypes_top.Ctypes_printers.format_off_t";
                 "Ctypes_top.Ctypes_printers.format_pid_t";
                 "Ctypes_top.Ctypes_printers.format_size_t";
                 "Ctypes_top.Ctypes_printers.format_ssize_t";
                 "Ctypes_top.Ctypes_printers.format_suseconds_t";
                 "Ctypes_top.Ctypes_printers.format_time_t";
                 "Ctypes_top.Ctypes_printers.format_uid_t";
                 "Ctypes_top.Ctypes_printers.format_useconds_t";]

let eval_string
      ?(print_outcome = true) ?(err_formatter = Format.err_formatter) str =
  let lexbuf = Lexing.from_string str in
  let phrase = !Toploop.parse_toplevel_phrase lexbuf in
  Toploop.execute_phrase print_outcome err_formatter phrase

let rec install_printers = function
  | [] -> true
  | printer :: printers ->
      let cmd = Printf.sprintf "#install_printer %s;;" printer in
      eval_string cmd && install_printers printers

let () =
  if not (install_printers printers) then
    Format.eprintf "Problem installing ctypes-printers@."
