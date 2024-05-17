(* Adapted from Anil Madhavapeddy's ocaml-uri package. *)


let printers = [ "fun fmt -> Ctypes.format_typ fmt";
                 "fun fmt -> Ctypes.format_fn fmt";
                 "Signed.SInt.pp";
                 "Signed.Long.pp";
                 "Signed.LLong.pp";
                 "Unsigned.UChar.pp";
                 "Unsigned.UInt8.pp";
                 "Unsigned.UInt16.pp";
                 "Unsigned.UInt32.pp";
                 "Unsigned.UInt64.pp";
                 "Unsigned.Size_t.pp";
                 "Unsigned.UShort.pp";
                 "Unsigned.UInt.pp";
                 "Unsigned.ULong.pp";
                 "Unsigned.ULLong.pp";
                 "fun fmt v -> let open Ctypes in
                    let typ = ptr (reference_type v) in
                    Format.fprintf fmt \"(%a) %a\" (fun fmt -> format_typ fmt) typ (format typ) v";
                 "fun fmt v ->  Ctypes.(format (reference_type (addr v)) fmt v)";
                 "fun fmt v ->  Ctypes.(format (reference_type (addr v)) fmt v)";
                 "fun fmt v ->  Ctypes.(format CArray.(array (length v) (reference_type (start v))) fmt v)";
                 "fun fmt (Ctypes_static.OCamlRef (_, _, ty) as v) ->  Ctypes.format (Ctypes_static.OCaml ty) fmt v";
                 "fun fmt v ->  Ctypes.format PosixTypes.clock_t fmt v";
                 "fun fmt v ->  Ctypes.format PosixTypes.dev_t fmt v";
                 "fun fmt v ->  Ctypes.format PosixTypes.ino_t fmt v";
                 "fun fmt v ->  Ctypes.format PosixTypes.mode_t fmt v";
                 "fun fmt v ->  Ctypes.format PosixTypes.nlink_t fmt v";
                 "fun fmt v ->  Ctypes.format PosixTypes.off_t fmt v";
                 "fun fmt v ->  Ctypes.format PosixTypes.pid_t fmt v";
                 "PosixTypes.Ssize.pp";
                 "PosixTypes.Time.pp";
                 "Ctypes.format PosixTypes.useconds_t";
                 "(fun fmt v -> Format.fprintf fmt \"<ldouble %s>\"
                                (LDouble.to_string v))";
                 "(fun fmt v -> Format.fprintf fmt \"<complexld %s + %si>\"
                                (LDouble.to_string (ComplexL.re v)) (LDouble.to_string (ComplexL.im v)))";

]

let eval_string
      ?(print_outcome = false) ?(err_formatter = Format.err_formatter) str =
  let lexbuf = Lexing.from_string str in
  let phrase = !Toploop.parse_toplevel_phrase lexbuf in
  Toploop.execute_phrase print_outcome err_formatter phrase

let install_printer printer = 
  begin
    ignore (eval_string (Printf.sprintf "let _printer = (%s);;" printer));
    ignore (eval_string "#install_printer _printer;;")
  end

let () = List.iter install_printer printers
