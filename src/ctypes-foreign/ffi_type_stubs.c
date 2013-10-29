/*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 */

#include <limits.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>

#include <ffi.h>

#include "../ctypes/raw_pointer.h"
#include "../ctypes/managed_buffer_stubs.h"

#if CHAR_MIN < 0
#define ctypes_ffi_type_char ffi_type_schar
#else
#define ctypes_ffi_type_char ffi_type_uchar 
#endif

/* We need a pointer-sized integer type.  SIZEOF_PTR is from caml/config.h. */
#if SIZEOF_PTR == 4
#define ctypes_ffi_type_camlint ffi_type_sint32
#elif SIZEOF_PTR == 8
#define ctypes_ffi_type_camlint ffi_type_sint64
#else
#error "No suitable pointer-sized integer type available"
#endif

/* long long is at least 64 bits. */
#if LLONG_MAX == 9223372036854775807LL
#define ctypes_ffi_type_sllong ffi_type_sint64
#define ctypes_ffi_type_ullong ffi_type_uint64
#else
# error "No suitable OCaml type available for representing longs"
#endif

#if SIZE_MAX == 65535U
#define ctypes_ffi_type_size_t ffi_type_uint16
#elif SIZE_MAX == 4294967295UL
#define ctypes_ffi_type_size_t ffi_type_uint32
#elif SIZE_MAX == 18446744073709551615ULL
#define ctypes_ffi_type_size_t ffi_type_uint64
#else
# error "No suitable OCaml type available for representing size_t values"
#endif

/* The order here must correspond to the constructor order in primitives.ml */
static ffi_type *primitive_ffi_types[] = {
  &ctypes_ffi_type_char,    /* Char */
  &ffi_type_schar,          /* Schar */
  &ffi_type_uchar,          /* Uchar */
  &ffi_type_sshort,         /* Short */
  &ffi_type_sint,           /* Int */
  &ffi_type_slong,          /* Long */
  &ctypes_ffi_type_sllong,  /* Llong */
  &ffi_type_ushort,         /* Ushort */
  &ffi_type_ulong,          /* Uint */
  &ffi_type_ulong,          /* Ulong */
  &ctypes_ffi_type_ullong,  /* Ullong */
  &ctypes_ffi_type_size_t,  /* Size */
  &ffi_type_sint8,          /* Int8 */
  &ffi_type_sint16,         /* Int16 */
  &ffi_type_sint32,         /* Int32 */
  &ffi_type_sint64,         /* Int64 */
  &ffi_type_uint8,          /* Uint8 */
  &ffi_type_uint16,         /* Uint16 */
  &ffi_type_uint32,         /* Uint32 */
  &ffi_type_uint64,         /* Uint64 */
  &ctypes_ffi_type_camlint, /* Camlint */
  &ctypes_ffi_type_camlint, /* Nativeint */
  &ffi_type_float,          /* Float */
  &ffi_type_double,         /* Double */
  NULL,                     /* Complex32 */
  NULL,                     /* Complex64 */
};


/* primitive_ffitype : 'a prim -> 'a ffitype */
value ctypes_primitive_ffitype(value prim)
{
  return CTYPES_FROM_PTR(primitive_ffi_types[Int_val(prim)]);
}

/* pointer_ffitype : unit -> voidp ffitype */
value ctypes_pointer_ffitype(value _)
{
  return CTYPES_FROM_PTR(&ffi_type_pointer);
}

/* void_ffitype : unit -> unit ffitype */
value ctypes_void_ffitype(value _)
{
  return CTYPES_FROM_PTR(&ffi_type_void);
}

#define Struct_ffitype_val(v) (*(ffi_type **)Data_custom_val(v))

/* allocate_struct_ffitype : int -> managed_buffer */
value ctypes_allocate_struct_ffitype(value nargs_)
{
  CAMLparam1(nargs_);

  int nargs = Int_val(nargs_);
  /* Space for the struct ffi_type plus a null-terminated array of arguments */
  int size = sizeof (ffi_type) + (1 + nargs) * sizeof (ffi_type *);
  CAMLlocal1(block);
  block = ctypes_allocate(Val_int(size));
  ffi_type *struct_type = Struct_ffitype_val(block);
  struct_type->size = 0;
  struct_type->alignment = 0;
  struct_type->type = FFI_TYPE_STRUCT;
  struct_type->elements = (ffi_type **)(struct_type + 1);
  struct_type->elements[nargs] = NULL;
  CAMLreturn (block);
}

/* struct_ffitype_set_argument : managed_buffer -> int -> _ ffitype -> unit */
value ctypes_struct_ffitype_set_argument(value struct_type_, value index_, value arg_)
{
  int index = Int_val(index_);
  ffi_type *arg = CTYPES_TO_PTR(arg_);

  ffi_type *struct_type = Struct_ffitype_val(struct_type_);
  struct_type->elements[index] = arg;
  return Val_unit;
}


extern void ctypes_check_ffi_status(ffi_status);

 /* complete_struct_type : managed_buffer -> unit */
value ctypes_complete_structspec(value struct_type_)
{
  ffi_cif _dummy_cif;
  ffi_type *struct_type = Struct_ffitype_val(struct_type_);

  ffi_status status = ffi_prep_cif(&_dummy_cif, FFI_DEFAULT_ABI, 0,
                                   struct_type, NULL);
  
  ctypes_check_ffi_status(status);

  return Val_unit;
}
