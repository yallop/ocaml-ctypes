module No_functions (F : Ctypes.FOREIGN) = struct end

let () = Tests_common.run Sys.argv
   ~structs:(module Types.Struct_stubs)
   (module No_functions)
