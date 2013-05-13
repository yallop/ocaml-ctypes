#include <limits.h>
#include <string.h>
#include <stdint.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>

#include <ffi.h>

#include "managed_buffer_stubs.h"
#include "unsigned_stubs.h"
#include "type_info_stubs.h"

#define make_primitive_interface(VAL_X, X_VAL, CTYPE, MUNGENAME, FFITYPE)      \
  static value raw_read_ ## MUNGENAME(struct type_info * _, void *p)           \
  {                                                                            \
    return VAL_X(*(CTYPE *)p);                                                 \
  }                                                                            \
                                                                               \
  static value raw_write_ ## MUNGENAME(struct type_info * _, void *p, value v) \
  {                                                                            \
    CAMLparam1(v);                                                             \
    *(CTYPE *)p = X_VAL(v);                                                    \
    CAMLreturn(Val_unit);                                                      \
  }                                                                            \
                                                                               \
  static struct type_info _ ## MUNGENAME ## _type_info = {                     \
    #MUNGENAME,                                                                \
    &FFITYPE,                                                                  \
    raw_read_ ## MUNGENAME,                                                    \
    raw_write_ ## MUNGENAME,                                                   \
  };                                                                           \
                                                                               \
  value ctypes_ ## MUNGENAME ## _type_info(value unit)                         \
  {                                                                            \
    CAMLparam1(unit);                                                          \
    CAMLlocal1(block);                                                         \
    block = allocate_custom(&type_info_custom_ops,                             \
                            sizeof(struct type_info),                          \
                            &_ ## MUNGENAME ## _type_info);                    \
    CAMLreturn (block);                                                        \
  }                                                                            \


/* TODO: inline or factor out */
static value allocate_custom(struct custom_operations *ops, size_t size, void *prototype)
{
  /* http://caml.inria.fr/pub/docs/manual-ocaml-4.00/manual033.html#htoc286 */
  value block = caml_alloc_custom(ops, size, 0, 1);
  void *data = Data_custom_val(block);
  if (prototype != NULL)
  {
    memcpy(data, prototype, size);
  }
  return block;
}


static value raw_read_struct(struct type_info * ti, void *buf)
{
  CAMLparam0();
  CAMLlocal1(block);
  block = ctypes_allocate(Val_int(ti->ffitype->size));
  memcpy(*((void **)Data_custom_val(block)), buf, ti->ffitype->size);
  CAMLreturn(block);
}


static value raw_write_struct(struct type_info * ti, void *buf, value cv)
{
  void **cv_ = Data_custom_val(cv);
  memcpy(buf, *cv_, ti->ffitype->size);
  return Val_unit;
}

static struct type_info _struct_type_info_prototype = {
  "struct",
  NULL,
  raw_read_struct,
  raw_write_struct,
};


static int compare_type_infos(value l_, value r_)
{
  struct type_info *lti = Data_custom_val(l_);
  struct type_info *rti = Data_custom_val(r_);

  intptr_t l = (intptr_t)&lti->ffitype;
  intptr_t r = (intptr_t)&rti->ffitype;
  return (l > r) - (l < r);
}


static long hash_type_info(value v)
{
  struct type_info *l = Data_custom_val(v);
  return (long)l->ffitype;
}


static struct custom_operations type_info_custom_ops = {
  "ocaml-ctypes:type_info",
  custom_finalize_default,
  compare_type_infos,
  hash_type_info,
  /* type_info objects are not serializable */
  custom_serialize_default,
  custom_deserialize_default
};

/* allocate_type_info : unit -> _ ctype */
value ctypes_allocate_type_info(value unit)
{
  return allocate_custom(&type_info_custom_ops,
                         sizeof(struct type_info),
                         &_struct_type_info_prototype);
}


struct struct_type_info {
  struct type_info type_info;
  ffi_type         ffi_type;
};


/* allocate_struct_type_info : ffitype** -> _ ctype */
value ctypes_allocate_struct_type_info(ffi_type **args)
{
  CAMLlocal1(block);
  block = allocate_custom(&type_info_custom_ops,
                          sizeof(struct struct_type_info),
                          &_struct_type_info_prototype);
  struct struct_type_info *t = Data_custom_val(block);
  /* TODO: this is not safe: we're keeping a pointer into a block that may be moved */
  ffi_type *s = t->type_info.ffitype = &t->ffi_type;

  s->type = FFI_TYPE_STRUCT;
  s->elements = args;
  s->size = s->alignment = 0;

  return block;
}


static value Cast_from_voidp(void *p) { return (value)p; }
static void *Cast_to_voidp(value p) { return (void *)p; }

make_primitive_interface(Val_int, Int_val, int8_t, int8_t, ffi_type_sint8)
make_primitive_interface(Val_int, Int_val, int16_t, int16_t, ffi_type_sint16)
make_primitive_interface(Val_int, Int_val, signed char, schar, ffi_type_schar)
#if CHAR_MIN < 0
make_primitive_interface(Val_int, Int_val, signed char, char, ffi_type_schar)
#else
make_primitive_interface(Val_int, Int_val, unsigned char, char, ffi_type_uchar)
#endif
make_primitive_interface(Val_int, Int_val, int, int, ffi_type_sint)
make_primitive_interface(Val_int, Int_val, short, short, ffi_type_sshort)
make_primitive_interface(caml_copy_int32, Int32_val, int32_t, int32_t, ffi_type_sint32)
make_primitive_interface(caml_copy_int64, Int64_val, int64_t, int64_t, ffi_type_sint64)
make_primitive_interface(caml_copy_double, Double_val, double, double, ffi_type_double)
make_primitive_interface(caml_copy_double, Double_val, float, float, ffi_type_float)
make_primitive_interface(Cast_from_voidp, Cast_to_voidp, void *, voidp, ffi_type_pointer)
make_primitive_interface(caml_copy_nativeint, Nativeint_val, int, nativeint, ffi_type_sint)

/* TODO: this is probably not really safe.  Receiving strings from C
   is fine, but passing strings to C means passing a pointer to an
   object that the GC may move.  Perhaps we can copy the string into
   safer storage (e.g. a managed_buffer) before the call.
 */
make_primitive_interface(caml_copy_string, String_val, char *, string, ffi_type_pointer)
#define Uint8_val(V) (*((uint8_t *) Data_custom_val(V)))
#define Uint16_val(V) (*((uint8_t *) Data_custom_val(V)))
#define Uint32_val(V) (*((uint8_t *) Data_custom_val(V)))
#define Uint64_val(V) (*((uint8_t *) Data_custom_val(V)))
make_primitive_interface(caml_copy_uint8, Uint8_val, uint8_t, uint8_t, ffi_type_uint8)
make_primitive_interface(caml_copy_uint16, Uint16_val, uint16_t, uint16_t, ffi_type_uint16)
make_primitive_interface(caml_copy_uint32, Uint32_val, uint32_t, uint32_t, ffi_type_uint32)
make_primitive_interface(caml_copy_uint64, Uint64_val, uint64_t, uint64_t, ffi_type_uint64)
make_primitive_interface(caml_copy_uint8, Uint8_val, unsigned char, uchar, ffi_type_uchar)

/* short is at least 16 bits. */
#if USHRT_MAX == 65535U
  make_primitive_interface(caml_copy_uint16, Uint16_val, unsigned short, ushort, ffi_type_uint16)
#elif USHRT_MAX == 4294967295UL
  make_primitive_interface(caml_copy_uint32, Uint32_val, unsigned short, ushort, ffi_type_uint32)
#elif USHRT_MAX == 18446744073709551615ULL
  make_primitive_interface(caml_copy_uint64, Uint64_val, unsigned short, ushort, ffi_type_uint64)
#else
# error "No suitable OCaml type available for representing unsigned short values"
#endif

/* int is at least 16 bits. */
#if UINT_MAX == 65535U
  make_primitive_interface(caml_copy_uint16, Uint16_val, unsigned int, uint, ffi_type_uint16)
#elif UINT_MAX == 4294967295UL
  make_primitive_interface(caml_copy_uint32, Uint32_val, unsigned int, uint, ffi_type_uint32)
#elif UINT_MAX == 18446744073709551615ULL
  make_primitive_interface(caml_copy_uint64, Uint64_val, unsigned int, uint, ffi_type_uint64)
#else
# error "No suitable OCaml type available for representing unsigned int values"
#endif

/* long is at least 32 bits. */
#if LONG_MAX == 2147483647L
  make_primitive_interface(caml_copy_int32, Int32_val, long, long, ffi_type_sint32)
  make_primitive_interface(caml_copy_uint32, Uint32_val, unsigned long, ulong, ffi_type_uint32)
#elif LONG_MAX == 9223372036854775807LL
  make_primitive_interface(caml_copy_int64, Int64_val, long, long, ffi_type_sint64)
  make_primitive_interface(caml_copy_uint64, Uint64_val, unsigned long, ulong, ffi_type_uint64)
#else
# error "No suitable OCaml type available for representing longs"
#endif

/* long long is at least 64 bits. */
#if LLONG_MAX == 9223372036854775807LL
  make_primitive_interface(caml_copy_int64, Int64_val, long long, llong, ffi_type_sint64)
  make_primitive_interface(caml_copy_uint64, Uint64_val, unsigned long long, ullong, ffi_type_uint64)
#else
# error "No suitable OCaml type available for representing longs"
#endif

#if SIZE_MAX == 65535U
  make_primitive_interface(caml_copy_uint16, Uint16_val, size_t, size_t, ffi_type_uint16)
#elif SIZE_MAX == 4294967295UL
  make_primitive_interface(caml_copy_uint32, Uint32_val, size_t, size_t, ffi_type_uint32)
#elif SIZE_MAX == 18446744073709551615ULL
  make_primitive_interface(caml_copy_uint64, Uint64_val, size_t, size_t, ffi_type_uint64)
#else
# error "No suitable OCaml type available for representing size_t values"
#endif

static value raw_read_void(struct type_info *ti, void *buf) { return Val_unit; }
static value raw_write_void(struct type_info *ti, void *buf, value cv) { return Val_unit; }

static struct type_info _void_type_info = {
  "void",
  &ffi_type_void,
  raw_read_void,
  raw_write_void,
};

value ctypes_void_type_info(value unit)
{
    CAMLparam1(unit);
    CAMLlocal1(block);
    block = ctypes_allocate_type_info(unit);
    struct type_info *ti = Data_custom_val(block);
    memcpy(ti, &_void_type_info, sizeof _void_type_info);
    CAMLreturn (block);
}


/* sizeof : _ ctype -> int */
value ctypes_sizeof(value typespec)
{
  return Val_int(((struct type_info *)(Data_custom_val(typespec)))->ffitype->size);
}


/* alignment : _ ctype -> int */
value ctypes_alignment(value typespec)
{
  return Val_int(((struct type_info *)(Data_custom_val(typespec)))->ffitype->alignment);
}


/* Read a C value from a block of memory */
/* read : ctype -> offset:int -> buffer -> 'a */
value ctypes_read(value ctype, value offset_, value buffer_)
{
  CAMLparam3(ctype, offset_, buffer_);
  struct type_info *type_info = Data_custom_val(ctype);
  int offset = Int_val(offset_);

  CAMLreturn(type_info->raw_read(type_info, (char *)buffer_ + offset));
}


/* Write a C value to a block of memory */
/* write : ctype -> offset:int -> 'a -> buffer -> unit */
value ctypes_write(value ctype, value offset_, value v, value buffer_)
{
  CAMLparam4(ctype, offset_, buffer_, v);
  struct type_info *type_info = Data_custom_val(ctype);
  int offset = Int_val(offset_);

  CAMLreturn(type_info->raw_write(type_info, (char *)buffer_ + offset, v));
}
