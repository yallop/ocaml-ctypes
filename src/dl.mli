type library

exception DL_error of string

type flag = 
    RTLD_LAZY
  | RTLD_NOW
  | RTLD_GLOBAL
  | RTLD_NODELETE
  | RTLD_NOLOAD
  | RTLD_DEEPBIND

val dlopen : ?filename:string -> flags:flag list -> library
val dlclose : handle:library -> unit
val dlsym : ?handle:library -> symbol:string -> Ctypes_raw.immediate_pointer
