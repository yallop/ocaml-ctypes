#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/hash.h>
#include <caml/unixsupport.h>

/* #include <caml/threads.h> */

#include <ffi.h>

#include <errno.h>
#include <assert.h>
#include <limits.h>
#include <string.h>

#include <stdint.h>

/* TODO: support callbacks that raise exceptions?  e.g. using caml_callback_exn etc.  */
/* TODO: thread support (caml_acquire_runtime_system / caml_release_runtime_system)
   (2) As a special case: If you enter/leave_blocking_section() then some
   other thread might (almost certainly will) do an allocation. So any ocaml
   values you need in enter/leave_blocking_section() need to be copied to C
   values beforehand and values you need after need to be protected. */
/* TODO: perhaps we should just use memcmp in places of [bits of] the
   tedious comparison functions */

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


struct type_info {
  const char *name;
  ffi_type   *ffitype;
  value     (*raw_read)(struct type_info *, void *);
  value     (*raw_write)(struct type_info *, void *, value);
};


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


/* _allocate_structure : _ctype -> voidp */
value ctypes_allocate(value size_)
{
  CAMLparam1(size_);
  int size = Int_val(size_);
  CAMLlocal1(block);
  block = allocate_custom(&managed_buffer_custom_ops, sizeof(void*), NULL);
  /* We don't want to let OCaml manage the block (i.e. allocate it
     with allocate_custom rather than malloc) because the GC is likely
     to move it about at inconvenient moments, rendering our pointers
     invalid. */
  void *p = malloc(size);
  if (p == NULL) {
    caml_raise_out_of_memory();
  }

  void **d = (void **)Data_custom_val(block);
  *d = p;
  CAMLreturn(block);
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
extern value caml_copy_uint8(uint8_t);
extern value caml_copy_uint16(uint16_t);
extern value caml_copy_uint32(uint32_t);
extern value caml_copy_uint64(uint64_t);
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

/* null_value : unit -> voidp */
value ctypes_null_value(value unit)
{
  return (value)NULL;
}

/* sizeof : _ctype -> int */
value ctypes_sizeof(value typespec)
{
  return Val_int(((struct type_info *)(Data_custom_val(typespec)))->ffitype->size);
}

/* alignment : _ctype -> int */
value ctypes_alignment(value typespec)
{
  return Val_int(((struct type_info *)(Data_custom_val(typespec)))->ffitype->alignment);
}

static void check_ffi_status(ffi_status status)
{
  switch (status)
  {
  case FFI_OK:
    break;
  case FFI_BAD_TYPEDEF:
    raise_with_string(*caml_named_value("FFI_internal_error"),
                      "FFI_BAD_TYPEDEF");
  case FFI_BAD_ABI:
    raise_with_string(*caml_named_value("FFI_internal_error"),
                      "FFI_BAD_ABI");
  default:
    assert(0);
  }
}

/* Given an offset into a fully-aligned buffer, compute the next
   offset that satisfies `alignment'. */
static size_t aligned_offset(size_t offset, size_t alignment)
{
  return offset + (offset % alignment);
}

/* A description of a typed buffer.  The bufferspec type serves a dual
   purpose: it describes the buffer used to hold the arguments that we
   pass to C functions via ffi_call, and it describes the layout of
   structs.
*/
static struct bufferspec {
  /* The ffi_cif structure holds the information that we're
     maintaining here, but it isn't part of the public interface. */

  /* The space needed to store properly-aligned arguments and return
     value. */
  size_t bytes;

  /* The number of elements. */
  size_t nelements;

  /* The capacity of the args array, including the terminating null. */
  size_t capacity;

  /* The state of the bufferspec value. */
  enum { BUILDING, STRUCTSPEC, CALLSPEC } state;

  /* A null-terminated array of size `nelements' types */
  ffi_type **args;

} bufferspec_prototype = {
  0, 0, 0, BUILDING, NULL,
};

static struct callspec {
  struct bufferspec bufferspec;

  /* return value offset */
  size_t roffset;

  /* The libffi call interface structure. */
  ffi_cif cif;
} callspec_prototype = {
  { 0, 0, 0, BUILDING, NULL }, -1,
};

void finalize_bufferspec(value v)
{
  struct bufferspec *bufferspec = Data_custom_val(v);
  free(bufferspec->args);
}


static int compare_bufferspecs(value l_, value r_)
{
  struct bufferspec *lti = Data_custom_val(l_);
  struct bufferspec *rti = Data_custom_val(r_);

  intptr_t l = (intptr_t)&lti->args;
  intptr_t r = (intptr_t)&rti->args;
  return (l > r) - (l < r);
}


static long hash_bufferspec(value v)
{
  struct bufferspec *bufferspec = Data_custom_val(v);

  return bufferspec->args != NULL
    ? caml_hash_mix_int64(0, (uint64)bufferspec->args)
    : 0;
}


static struct custom_operations bufferspec_custom_ops = {
  "ocaml-ctypes:bufferspec",
  finalize_bufferspec,
  compare_bufferspecs,
  hash_bufferspec,
  /* bufferspec objects are not serializable */
  custom_serialize_default,
  custom_deserialize_default
};

/* We store two things in the callbuffer: a "scratch" area for passing
   arguments and receiving the return value, and an array of pointers into the
   scratch area; we pass that array to ffi_call along with a pointer to the
   return value space.

   The scratch area comes first, followed by the pointer array.

   The incomplete struct type gives a modicum of type safety over void *: the
   compiler should reject incompatible assignments, for example.
 */
typedef struct callbuffer callbuffer;

/* Read a C value from a block of memory */
/* read : int -> ctype -> buffer -> 'a */
value ctypes_read(value offset_, value ctype, value buffer_)
{
  CAMLparam3(offset_, ctype, buffer_);
  struct type_info *type_info = Data_custom_val(ctype);
  callbuffer *buffer = (callbuffer *)buffer_;
  int offset = Int_val(offset_);

  CAMLreturn(type_info->raw_read(type_info, (char *)buffer + offset));
}

/* Write a C value to a block of memory */
/* write : int -> ctype -> buffer -> 'a -> unit */
value ctypes_write(value offset_, value ctype, value v, value buffer_)
{
  CAMLparam4(offset_, ctype, buffer_, v);
  struct type_info *type_info = Data_custom_val(ctype);
  callbuffer *buffer = (callbuffer *)buffer_;
  int offset = Int_val(offset_);

  CAMLreturn(type_info->raw_write(type_info, (char *)buffer + offset, v));
}

/* [TODO]: document. */
static size_t compute_arg_buffer_size(struct bufferspec *bufferspec, size_t *arg_array_offset)
{
  assert(bufferspec->state == CALLSPEC);

  size_t bytes = bufferspec->bytes;

  size_t callbuffer_size = bytes;
  *arg_array_offset = aligned_offset(bytes, ffi_type_pointer.alignment);
  bytes = *arg_array_offset + bufferspec->nelements * sizeof(void *);

  return bytes;
}

/* [TODO]: document. */
static void populate_callbuffer(struct bufferspec *bufferspec, callbuffer *buf, size_t bytes, size_t arg_array_offset)
{
  void **arg_array = (void **)((char *)buf + arg_array_offset);
  size_t i = 0, offset = 0;
  for (; i < bufferspec->nelements; i++)
  {
    offset = aligned_offset(offset, bufferspec->args[i]->alignment);
    arg_array[i] = (char *)buf + offset;
    offset += bufferspec->args[i]->size;
  }
}


/* Allocate a new C buffer specification */
/* allocate_buffer : unit -> bufferspec */
value ctypes_allocate_bufferspec(value unit)
{
  return allocate_custom(&bufferspec_custom_ops,
                         sizeof(struct bufferspec),
                         &bufferspec_prototype);
}

/* Allocate a new C call specification */
/* allocate_callspec : unit -> callspec */
value ctypes_allocate_callspec(value unit)
{
  return allocate_custom(&bufferspec_custom_ops,
                         sizeof(struct callspec),
                         &bufferspec_prototype);
}

/* Add an argument to the C call specification */
/* add_argument : bufferspec -> 'a ctype -> int */
value ctypes_add_argument(value bufferspec_, value argument_)
{
  static const size_t increment_size = 8;

  CAMLparam2(bufferspec_, argument_);
  struct bufferspec *bufferspec = Data_custom_val(bufferspec_);
  ffi_type *argtype = (((struct type_info *)Data_custom_val(argument_)))->ffitype;

  assert(bufferspec->state == BUILDING); /* TODO: raise a proper error */

  int offset = aligned_offset(bufferspec->bytes, argtype->alignment);
  bufferspec->bytes = offset + argtype->size;

  if (bufferspec->nelements + 2 >= bufferspec->capacity) {
    void *temp = realloc(bufferspec->args, (bufferspec->capacity + increment_size) * sizeof *bufferspec->args);
    if (temp == NULL) {
      caml_raise_out_of_memory();
    }
    bufferspec->args = temp;
    bufferspec->capacity += increment_size;
  }
  bufferspec->args[bufferspec->nelements] = argtype;
  bufferspec->args[bufferspec->nelements + 1] = NULL;
  bufferspec->nelements += 1;

  CAMLreturn(Val_int(offset));
}

/* Pass the return type and conclude the specification preparation */
/* prep_callspec : bufferspec -> 'a ctype -> unit */
value ctypes_prep_callspec(value callspec_, value rtype)
{
  CAMLparam2(callspec_, rtype);

  struct callspec *callspec = Data_custom_val(callspec_);
  ffi_type *rffitype = (((struct type_info *)Data_custom_val(rtype)))->ffitype;

  /* Add the (aligned) space needed for the return value */
  callspec->roffset = aligned_offset(callspec->bufferspec.bytes, rffitype->alignment);
  callspec->bufferspec.bytes = callspec->roffset + rffitype->size;

  /* Allocate an extra word after the return value space to work
     around a bug in libffi which causes it to write past the return
     value space.

        https://github.com/atgreen/libffi/issues/35
  */
  callspec->bufferspec.bytes = aligned_offset(callspec->bufferspec.bytes, ffi_type_pointer.alignment);
  callspec->bufferspec.bytes += ffi_type_pointer.size;

  ffi_status status = ffi_prep_cif(&callspec->cif,
                                   FFI_DEFAULT_ABI,
                                   callspec->bufferspec.nelements,
                                   rffitype,
                                   callspec->bufferspec.args);

  check_ffi_status(status);

  callspec->bufferspec.state = CALLSPEC;
  CAMLreturn(Val_unit);
}

/* Call the function specified by `callspec', passing arguments and return
   values in `buffer' */
/* call' : voidp -> callspec -> (buffer -> unit) -> (buffer -> 'a) -> 'a */
value ctypes_call(value function, value callspec_, value argwriter, value rvreader)
{
  CAMLparam4(function, callspec_, argwriter, rvreader);

  void (*cfunction)(void) = (void (*)(void))(void *)function;
  struct callspec *callspec = Data_custom_val(callspec_);
  struct bufferspec *bufferspec = (struct bufferspec *)callspec;
  int roffset = callspec->roffset;

  assert(bufferspec->state == CALLSPEC);

  size_t arg_array_offset;
  size_t bytes = compute_arg_buffer_size(bufferspec, &arg_array_offset);

  char *buffer = alloca(bytes);
  char *return_slot = buffer + roffset;

  populate_callbuffer(bufferspec, (struct callbuffer *)buffer, bytes, arg_array_offset);

  caml_callback(argwriter, (value)buffer);

  ffi_call(&callspec->cif,
           cfunction,
           return_slot,
           (void **)(buffer + aligned_offset(bufferspec->bytes, ffi_type_pointer.alignment)));

  CAMLreturn(caml_callback(rvreader, (value)return_slot));
}

value ctypes_call_errno(value fnname, value function, value callspec_, value argwriter, value rvreader)
{
  CAMLparam5(fnname, function, callspec_, argwriter, rvreader);
  
  errno = 0;
  CAMLlocal1(rv);
  rv = ctypes_call(function, callspec_, argwriter, rvreader);
  if (errno != 0)
  {
    char *buffer = alloca(caml_string_length(fnname) + 1);
    strcpy(buffer, String_val(fnname));
    unix_error(errno, buffer, Nothing);
  }
  CAMLreturn(rv);
}


typedef struct closure closure;
struct closure
{
  ffi_closure closure;
  value       fn;
};

enum boxedfn_tags { Done, Fn };

static void callback_handler(ffi_cif *cif,
                             void *ret,
                             void **args,
                             void *user_data_)
{
  int arity = cif->nargs;

  value *user_data = user_data_;

  CAMLlocal1(boxedfn);
  boxedfn = *user_data;

  int i;
  for (i = 0; i < arity; i++)
  {
    void *cvalue = args[i];
    assert (Tag_val(boxedfn) == Fn);
    /* unbox and call */
    boxedfn = caml_callback(Field(boxedfn, 0), (value)cvalue);
  }

  /* now store the return value */
  assert (Tag_val(boxedfn) == Done);
  caml_callback(Field(boxedfn, 0), (value)ret);
}


/* Construct a pointer to a boxed n-ary function */
/* make_function_pointer : callspec -> boxedfn -> voidp */
value ctypes_make_function_pointer(value callspec_, value boxedfn)
{
  CAMLparam2(callspec_, boxedfn);
  struct callspec *callspec = Data_custom_val(callspec_);

  assert(callspec->bufferspec.state == CALLSPEC);

  void (*code_address)(void) = NULL;

  closure *closure = ffi_closure_alloc(sizeof *closure,
                                       (void *)&code_address);

  if (closure == NULL) {
    caml_raise_out_of_memory();
  }
  else {
    closure->fn = boxedfn;
   /* TODO: what should be the lifetime of OCaml functions passed to C
      (ffi_closure objects) When can we call ffi_closure_free and
      caml_unregister_generational_global_root? */
    caml_register_generational_global_root(&(closure->fn));

    ffi_status status =  ffi_prep_closure_loc
      ((ffi_closure *)closure,
       &callspec->cif,
       callback_handler,
       &closure->fn,
       (void *)code_address);

    check_ffi_status(status);

    CAMLreturn ((value)(void *)code_address);
  }
}


/* _complete_struct_type : callspec -> _ctype */
value ctypes_complete_structspec(value bufferspec_)
{
  CAMLparam1(bufferspec_);

  struct bufferspec *bufferspec = Data_custom_val(bufferspec_);

  /* We'll use prep_ffi_cif to trigger computation of the size and alignment
     of the struct type rather than repeating what's already in libffi.  It'd
     be nice if the initialize_aggregate function were exposed so that we
     could do without the dummy cif.
  */
  ffi_cif _dummy_cif;

  CAMLlocal1(block);
  block = allocate_custom(&type_info_custom_ops,
                          sizeof(struct struct_type_info),
                          &_struct_type_info_prototype);
  struct struct_type_info *t = Data_custom_val(block);
  /* TODO: this is not safe: we're keeping a pointer into a block that may be moved */
  ffi_type *s = t->type_info.ffitype = &t->ffi_type;

  s->type = FFI_TYPE_STRUCT;
  s->elements = bufferspec->args;
  s->size = s->alignment = 0;

  ffi_status status = ffi_prep_cif(&_dummy_cif, FFI_DEFAULT_ABI, 0, s, NULL);

  check_ffi_status(status);

  bufferspec->state = STRUCTSPEC;


  CAMLreturn(block);
}

value ctypes_managed_secret(value managed_buffer)
{
  return (value)(*(void **)Data_custom_val(managed_buffer));
}

/* pointer_plus : char* -> int -> char* */
value ctypes_pointer_plus(value ptr, value i)
{
  /* TODO: we should perhaps check that the result is word-aligned */
  return (value)(((char*)ptr) + Int_val(i));
}


/* memcpy : dest:immediate_pointer -> dest_offset:int ->
            src:immediate_pointer -> src_offset:int ->
            size:int -> unit */
value ctypes_memcpy(value dst, value dst_offset,
                    value src, value src_offset, value size)
{
  CAMLparam5(dst, dst_offset, src, src_offset, size);
  memcpy((char *)dst + Int_val(dst_offset),
         (void *)src + Int_val(src_offset),
         Int_val(size));
  CAMLreturn(Val_unit);
}
