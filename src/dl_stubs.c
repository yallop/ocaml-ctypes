#define _GNU_SOURCE
#include <dlfcn.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>

#include <assert.h>

#define Val_none Val_int(0)
#define Some_val(v) Field(v, 0)

enum dl_flags_caml {
  _RTLD_LAZY,
  _RTLD_NOW,
  _RTLD_GLOBAL,
  _RTLD_NODELETE,
  _RTLD_NOLOAD,
#ifdef _RTLD_DEEPBIND
  _RTLD_DEEPBIND,
#endif /* _RTLD_DEEPBIND */
};

static value Val_some(value v)
{
    CAMLparam1(v);
    CAMLlocal1(some);
    some = caml_alloc(1, 0);
    Store_field(some, 0, v);
    CAMLreturn(some);
}

/* ctypes_resolve_dl_flag : flag -> int */
value ctypes_resolve_dl_flag(value flag)
{
  switch (Int_val(flag))
  {
    case _RTLD_LAZY:     return RTLD_LAZY;
    case _RTLD_NOW:      return RTLD_NOW;
    case _RTLD_GLOBAL:   return RTLD_GLOBAL;
    case _RTLD_NODELETE: return RTLD_NODELETE;
    case _RTLD_NOLOAD:   return RTLD_NOLOAD;
#ifdef _RTLD_DEEPBIND
    case _RTLD_DEEPBIND: return RTLD_DEEPBIND;
#endif /* _RTLD_DEEPBIND */
    default: assert(0);
  }
}

/* ctypes_dlopen : filename:string -> flags:int -> library option */
value ctypes_dlopen(value filename, value flag)
{
  CAMLparam2(filename, flag);

  char *cfilename = filename == Val_none ? NULL : String_val(Some_val(filename));
  int cflag = Int_val(flag);

  void *handle = dlopen(cfilename, cflag);
  CAMLreturn (handle != NULL ? Val_some((value)handle) : Val_none);
}

/* ctypes_dlsym : ?handle:library -> symbol:string -> cvalue option */
value ctypes_dlsym(value handle_option, value symbol)
{
  CAMLparam2(handle_option, symbol);

  void *handle = handle_option == Val_none ? RTLD_DEFAULT : (void *)Some_val(handle_option);

  char *s = String_val(symbol);
  void *result = dlsym(handle, s);
  CAMLreturn(result == NULL ? Val_none : Val_some((value)result));
}


/* ctypes_dlclose : handle:library -> int */
value ctypes_dlclose(value handle)
{
  CAMLparam1(handle);
  return Val_int(dlclose((void *)handle));
}

/* ctypes_dlerror : unit -> string option */
value ctypes_dlerror(value unit)
{
  CAMLparam1(unit);
  const char *error = dlerror();
  CAMLreturn (error != NULL ? Val_some(caml_copy_string(error)) : Val_none);
}
