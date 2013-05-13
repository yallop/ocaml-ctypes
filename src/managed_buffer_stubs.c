#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/custom.h>
#include <caml/fail.h>

#include <stdint.h>

static void finalize_free(value v)
{
  free(*((void **)Data_custom_val(v)));
}

static int compare_pointers(value l_, value r_)
{
  /* pointer comparison */
  intptr_t l = (intptr_t)*(void **)Data_custom_val(l_);
  intptr_t r = (intptr_t)*(void **)Data_custom_val(r_);
  return (l > r) - (l < r);
}

static long hash_address(value l)
{
  /* address hashing */
  return (long)*(void **)Data_custom_val(l);
}

static struct custom_operations managed_buffer_custom_ops = {
  "ocaml-ctypes:managed_buffer",
  finalize_free,
  compare_pointers,
  hash_address,
  /* Managed buffers are not serializable. */
  custom_serialize_default,
  custom_deserialize_default
};

/* allocate : int -> managed_buffer */
value ctypes_allocate(value size_)
{
  CAMLparam1(size_);
  int size = Int_val(size_);
  CAMLlocal1(block);
  block = caml_alloc_custom(&managed_buffer_custom_ops, sizeof(void*), 0, 1);
  void *p = malloc(size);
  if (p == NULL) {
    caml_raise_out_of_memory();
  }
  void **d = (void **)Data_custom_val(block);
  *d = p;
  CAMLreturn(block);
}

/* block_address : managed_buffer -> immediate_pointer */
value ctypes_block_address(value managed_buffer)
{
  return (value)(*(void **)Data_custom_val(managed_buffer));
}
