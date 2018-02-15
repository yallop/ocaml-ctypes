open Base
open Stdio

let () =
  let module C = Configurator in
  C.main ~name:"ffi" (fun c ->
      let default : C.Pkg_config.package_conf = {
        libs = ["-lffi"];
        cflags = []
      } in
      let conf =
        match C.Pkg_config.get c with
        | None -> default
        | Some pc ->
          Option.value (C.Pkg_config.query pc ~package:"libffi") ~default
      in
      let write_sexp file sexp =
        Out_channel.write_all file ~data:(Sexp.to_string sexp)
      in
      write_sexp "c_flags.sexp" (sexp_of_list sexp_of_string conf.cflags);
      write_sexp "c_library_flags.sexp" (sexp_of_list sexp_of_string conf.libs))
