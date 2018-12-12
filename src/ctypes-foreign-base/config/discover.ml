module C = Configurator.V1

let () =
  C.main ~name:"ffi" (fun c ->
      let default : C.Pkg_config.package_conf = {
        libs = ["-lffi"];
        cflags = []
      } in
      let conf =
        match C.Pkg_config.get c with
        | None -> default
        | Some pc ->
          (match C.Pkg_config.query pc ~package:"libffi") with
           | None -> default
           | Some v -> v)
      in
      let write_sexp file sexp =
        Out_channel.write_all file ~data:(Sexp.to_string sexp)
      in
      let backend =
        match C.ocaml_config_var_exn c "system" with
        | "Win32" -> "win"
        | _ -> "unix" in
      write_sexp "c_flags.sexp" (sexp_of_list sexp_of_string conf.cflags);
      write_sexp "c_library_flags.sexp" (sexp_of_list sexp_of_string conf.libs);
      write_sexp "backend.sexp" (sexp_of_string backend)
    )
