/*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 */

#include <limits.h>
#include <string.h>
#include <stdint.h>
#include <inttypes.h>
#include <stdio.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/custom.h>
#include <caml/alloc.h>

#include <ffi.h>

#include "managed_buffer_stubs.h"
#include "unsigned_stubs.h"
#include "type_info_stubs.h"
#include "raw_pointer.h"

#define make_primitive_interface(VAL_X, X_VAL, CTYPE, MUNGENAME, FFITYPE, FMT) \
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
  static value format_ ## MUNGENAME(struct type_info * _, value v)             \
  {                                                                            \
    CAMLparam1(v);                                                             \
    CTYPE t = X_VAL(v);                                                        \
    char buf[32];                                                              \
    snprintf(buf, sizeof buf, FMT, t);                                         \
    CAMLreturn (caml_copy_string(buf));                                        \
  }                                                                            \
                                                                               \
  static struct type_info _ ## MUNGENAME ## _type_info = {                     \
    #CTYPE,                                                                    \
    PASSABLE,                                                                  \
    &FFITYPE,                                                                  \
    raw_read_ ## MUNGENAME,                                                    \
    raw_write_ ## MUNGENAME,                                                   \
    format_ ## MUNGENAME,                                                      \
  };                                                                           \
                                                                               \
  value ctypes_ ## MUNGENAME ## _type_info(value unit)                         \
  {                                                                            \
    return ctypes_allocate_type_info(&_ ## MUNGENAME ## _type_info);           \
  }                                                                            \


static value allocate_custom(struct custom_operations *ops, size_t size,
                             void *prototype)
{
  /* http://caml.inria.fr/pub/docs/manual-ocaml-4.00/manual033.html#htoc286 */
  value block = caml_alloc_custom(ops, size, 0, 1);
  memcpy(Data_custom_val(block), prototype, size);
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


static value format_struct(struct type_info * ti, value v)
{
  return caml_copy_string("<struct>");
}


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


value ctypes_allocate_type_info(struct type_info *ti)
{
  return allocate_custom(&type_info_custom_ops, sizeof *ti, ti);
}


static struct struct_type_info {
  struct type_info type_info;
  ffi_type       **args;
} _struct_type_info_prototype = {
  { "struct",
    STRUCT,
    NULL,
    raw_read_struct,
    raw_write_struct,
    format_struct, },
  NULL,
};


static void finalize_struct_type_info(value v)
{
  struct struct_type_info *sti = Data_custom_val(v);
  free(sti->args);
  free(sti->type_info.ffitype);
}


static struct custom_operations struct_type_info_custom_ops = {
  /* the same as type_info, except for the finalizer */
  "ocaml-ctypes:struct_type_info",
  finalize_struct_type_info,
  compare_type_infos,
  hash_type_info,
  /* type_info objects are not serializable */
  custom_serialize_default,
  custom_deserialize_default
};


/* allocate_struct_type_info : ffitype*** -> _ ctype */
value ctypes_allocate_struct_type_info(ffi_type ***args)
{
  value block = allocate_custom(&struct_type_info_custom_ops,
                                sizeof(struct struct_type_info),
                                &_struct_type_info_prototype);

  struct struct_type_info *t = Data_custom_val(block);

  ffi_type *s = t->type_info.ffitype = caml_stat_alloc(sizeof *s);

  s->type = FFI_TYPE_STRUCT;
  s->size = s->alignment = 0;
  /* Assume ownership of the *alloced buffer 'args' */
  /* TODO: we could free the args/elements array at this point if the
     struct isn't passable */
  s->elements = t->args = *args;
  *args = NULL;

  return block;
}


make_primitive_interface(Val_int, Int_val, int8_t, int8_t, ffi_type_sint8, "%" PRId8)
make_primitive_interface(Val_int, Int_val, int16_t, int16_t, ffi_type_sint16, "%" PRId16)
make_primitive_interface(Val_int, Int_val, signed char, schar, ffi_type_schar, "%d")
#if CHAR_MIN < 0
  make_primitive_interface(Val_int, Int_val, char, char, ffi_type_schar, "'%c'")
#else
  make_primitive_interface(Val_int, Int_val, char, char, ffi_type_uchar, "'%c'")
#endif
make_primitive_interface(Val_int, Int_val, int, int, ffi_type_sint, "%d")
make_primitive_interface(Val_int, Int_val, short, short, ffi_type_sshort, "%hd")
make_primitive_interface(caml_copy_int32, Int32_val, int32_t, int32_t, ffi_type_sint32, "%" PRId32)
make_primitive_interface(caml_copy_int64, Int64_val, int64_t, int64_t, ffi_type_sint64, "%" PRId64)
make_primitive_interface(caml_copy_double, Double_val, double, double, ffi_type_double, "%.12g")
make_primitive_interface(caml_copy_double, Double_val, float, float, ffi_type_float, "%.12g")
make_primitive_interface(CTYPES_FROM_PTR, CTYPES_TO_PTR, void *, voidp, ffi_type_pointer, "%p")
make_primitive_interface(ctypes_copy_uint8, Uint8_val, uint8_t, uint8_t, ffi_type_uint8, "%" PRIu8)
make_primitive_interface(ctypes_copy_uint16, Uint16_val, uint16_t, uint16_t, ffi_type_uint16, "%" PRIu16)
make_primitive_interface(ctypes_copy_uint32, Uint32_val, uint32_t, uint32_t, ffi_type_uint32, "%" PRIu32)
make_primitive_interface(ctypes_copy_uint64, Uint64_val, uint64_t, uint64_t, ffi_type_uint64, "%" PRIu64)
make_primitive_interface(ctypes_copy_uint8, Uint8_val, unsigned char, uchar, ffi_type_uchar, "%d")

/* We need a pointer-sized integer type.  SIZEOF_PTR is from caml/config.h. */
#if SIZEOF_PTR == 4
#define ctypes_ffi_type_camlint ffi_type_sint32
#elif SIZEOF_PTR == 8
#define ctypes_ffi_type_camlint ffi_type_sint64
#else
#error "No suitable pointer-sized integer type available"
#endif
make_primitive_interface(Val_int, Int_val, intnat, camlint, ctypes_ffi_type_camlint,
                         "%" ARCH_INTNAT_PRINTF_FORMAT "d")
make_primitive_interface(caml_copy_nativeint, Nativeint_val, intnat, nativeint,
                         ctypes_ffi_type_camlint,
                         "%" ARCH_INTNAT_PRINTF_FORMAT "d")

/* short is at least 16 bits. */
#if USHRT_MAX == 65535U
  make_primitive_interface(ctypes_copy_uint16, Uint16_val, unsigned short, ushort, ffi_type_uint16, "%hu")
#elif USHRT_MAX == 4294967295UL
  make_primitive_interface(ctypes_copy_uint32, Uint32_val, unsigned short, ushort, ffi_type_uint32, "%hu")
#elif USHRT_MAX == 18446744073709551615ULL
  make_primitive_interface(ctypes_copy_uint64, Uint64_val, unsigned short, ushort, ffi_type_uint64, "%hu")
#else
# error "No suitable OCaml type available for representing unsigned short values"
#endif

/* int is at least 16 bits. */
#if UINT_MAX == 65535U
  make_primitive_interface(ctypes_copy_uint16, Uint16_val, unsigned int, uint, ffi_type_uint16, "%u")
#elif UINT_MAX == 4294967295UL
  make_primitive_interface(ctypes_copy_uint32, Uint32_val, unsigned int, uint, ffi_type_uint32, "%u")
#elif UINT_MAX == 18446744073709551615ULL
  make_primitive_interface(ctypes_copy_uint64, Uint64_val, unsigned int, uint, ffi_type_uint64, "%u")
#else
# error "No suitable OCaml type available for representing unsigned int values"
#endif

/* long is at least 32 bits. */
#if LONG_MAX == 2147483647L
  make_primitive_interface(caml_copy_int32, Int32_val, long, long, ffi_type_sint32, "%ld")
  make_primitive_interface(ctypes_copy_uint32, Uint32_val, unsigned long, ulong, ffi_type_uint32, "%lu")
#elif LONG_MAX == 9223372036854775807LL
  make_primitive_interface(caml_copy_int64, Int64_val, long, long, ffi_type_sint64, "%ld")
  make_primitive_interface(ctypes_copy_uint64, Uint64_val, unsigned long, ulong, ffi_type_uint64, "%lu")
#else
# error "No suitable OCaml type available for representing longs"
#endif

/* long long is at least 64 bits. */
#if LLONG_MAX == 9223372036854775807LL
  make_primitive_interface(caml_copy_int64, Int64_val, long long, llong, ffi_type_sint64, "%lld")
  make_primitive_interface(ctypes_copy_uint64, Uint64_val, unsigned long long, ullong, ffi_type_uint64, "%llu")
#else
# error "No suitable OCaml type available for representing longs"
#endif

#if SIZE_MAX == 65535U
  make_primitive_interface(ctypes_copy_uint16, Uint16_val, size_t, size_t, ffi_type_uint16, "%zu")
#elif SIZE_MAX == 4294967295UL
  make_primitive_interface(ctypes_copy_uint32, Uint32_val, size_t, size_t, ffi_type_uint32, "%zu")
#elif SIZE_MAX == 18446744073709551615ULL
  make_primitive_interface(ctypes_copy_uint64, Uint64_val, size_t, size_t, ffi_type_uint64, "%zu")
#else
# error "No suitable OCaml type available for representing size_t values"
#endif

static value raw_read_void(struct type_info *ti, void *buf) { return Val_unit; }
static value raw_write_void(struct type_info *ti, void *buf, value cv) { return Val_unit; }
static value format_void(struct type_info *ti, value cv) { return caml_copy_string(""); }

static struct type_info _void_type_info = {
  "void",
  UNPASSABLE,
  &ffi_type_void,
  raw_read_void,
  raw_write_void,
  format_void,
};


value ctypes_void_type_info(value unit)
{
  return ctypes_allocate_type_info(&_void_type_info);
}


/* passable : _ ctype -> bool */
value ctypes_passable(value typespec)
{
  struct type_info *ti = (struct type_info *)Data_custom_val(typespec);
  return Val_int(ti->passable == PASSABLE);
}


/* sizeof : _ ctype -> int */
value ctypes_sizeof(value typespec)
{
  struct type_info *ti = (struct type_info *)Data_custom_val(typespec);
  return Val_int(ti->ffitype->size);
}


/* alignment : _ ctype -> int */
value ctypes_alignment(value typespec)
{
  struct type_info *ti = (struct type_info *)Data_custom_val(typespec);
  return Val_int(ti->ffitype->alignment);
}


/* Read a C value from a block of memory */
/* read : 'a ctype -> offset:int -> immediate_pointer -> 'a */
value ctypes_read(value ctype, value offset_, value buffer_)
{
  CAMLparam3(ctype, offset_, buffer_);
  struct type_info *type_info = Data_custom_val(ctype);
  int offset = Int_val(offset_);

  CAMLreturn(type_info->raw_read(type_info, (char *)CTYPES_TO_PTR(buffer_) + offset));
}


/* Write a C value to a block of memory */
/* write : 'a ctype -> offset:int -> 'a -> immediate_pointer -> unit */
value ctypes_write(value ctype, value offset_, value v, value buffer_)
{
  CAMLparam4(ctype, offset_, buffer_, v);
  struct type_info *type_info = Data_custom_val(ctype);
  int offset = Int_val(offset_);

  CAMLreturn(type_info->raw_write(type_info, (char *)CTYPES_TO_PTR(buffer_) + offset, v));
}


/* Write a C value to a block of memory */
/* string_of : 'a ctype -> 'a -> string */
value ctypes_string_of(value ctype, value v)
{
  CAMLparam2(ctype, v);
  struct type_info *type_info = Data_custom_val(ctype);
  CAMLreturn(type_info->format(type_info, v));
}


/* Return the name of a type */
/* typename : _ ctype -> string */
value ctypes_typename(value ctype)
{
  CAMLparam1(ctype);
  struct type_info *type_info = Data_custom_val(ctype);

  CAMLreturn(caml_copy_string(type_info->name));
}
