#include <caml/memory.h>
#include <caml/mlvalues.h>
#include "ctypes_raw_pointer.h"

/* 'a -> voidp */
value ctypes_caml_roots_create(value v)
{
  caml_root *p = caml_stat_alloc(sizeof *p);
  *p = caml_create_root(v);
  return CTYPES_FROM_PTR(p);
}

/* voidp -> 'a -> unit */
value ctypes_caml_roots_set(value p_, value v)
{
  caml_root *p = CTYPES_TO_PTR(p_);
  caml_modify_root(*p, v);
  return Val_unit;
}

/* voidp -> 'a */
value ctypes_caml_roots_get(value p_)
{
  caml_root *p = CTYPES_TO_PTR(p_);
  return caml_read_root(*p);
}

/* voidp -> unit */
value ctypes_caml_roots_release(value p_)
{
  caml_root *p = CTYPES_TO_PTR(p_);
  caml_delete_root(*p);
  caml_stat_free(p);
  return Val_unit;
}

/* 'a -> unit */
value ctypes_use(value v)
{
  return v;
}
