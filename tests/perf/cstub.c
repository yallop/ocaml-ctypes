#include <caml/mlvalues.h>
#include <caml/memory.h>

extern int add_stuff_up(int);

CAMLprim value add_int_slow(value v)
{
  CAMLparam1(v);
  CAMLlocal1(res);
  res = Val_int(add_stuff_up(Int_val(v)));
  CAMLreturn(res);
}

CAMLprim value add_int_fast(value v)
{
  return Val_int(add_stuff_up(Int_val(v)));
}
