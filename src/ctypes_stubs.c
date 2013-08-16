/*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 */

#include <errno.h>
#include <assert.h>
#include <string.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/hash.h>
#include <caml/unixsupport.h>

#include <ffi.h>

#include "managed_buffer_stubs.h"
#include "type_info_stubs.h"
#include "raw_pointer.h"

/* TODO: support callbacks that raise exceptions?  e.g. using
   caml_callback_exn etc.  */

/* TODO: thread support */

/* An OCaml function that converts resolves identifiers to OCaml functions */
static value retrieve_closure_;

/* Resolve identifiers to OCaml functions */
static value retrieve_closure(int key)
{
  CAMLparam0 ();
  CAMLlocal1(result);
  result = caml_callback_exn(retrieve_closure_, Val_int(key));

  if (Is_exception_result(result)) {
    caml_raise_constant(*caml_named_value("CallToExpiredClosure"));
  }

  CAMLreturn (result);
}

/* Register the function used to resolve closure identifiers */ 
/* set_closure_callback : (int -> boxedfn) -> unit */
value ctypes_set_closure_callback(value retrieve)
{
  CAMLparam1(retrieve);

  caml_register_global_root(&retrieve_closure_);
  retrieve_closure_ = retrieve;

  CAMLreturn(Val_unit);  
}


static value allocate_custom(struct custom_operations *ops, size_t size,
                             void *prototype)
{
  /* http://caml.inria.fr/pub/docs/manual-ocaml-4.00/manual033.html#htoc286 */
  value block = caml_alloc_custom(ops, size, 0, 1);
  memcpy(Data_custom_val(block), prototype, size);
  return block;
}


void ctypes_check_ffi_status(ffi_status status)
{
  switch (status) {
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
  size_t overhang = offset % alignment;
  return overhang == 0
    ? offset
    : offset - overhang + alignment;
}


/* A description of a typed buffer.  The bufferspec type serves a dual
   purpose: it describes the buffer used to hold the arguments that we pass to
   C functions via ffi_call, and it describes the layout of structs.  */
static struct bufferspec {
  /* The ffi_cif structure holds the information that we're maintaining here,
     but it isn't part of the public interface. */
     
  /* The space needed to store properly-aligned arguments and return value. */
  size_t bytes;
     
  /* The number of elements. */
  size_t nelements;
  
  /* The capacity of the args array, including the terminating null. */
  size_t capacity;
  
  /* The maximum element alignment */
  size_t max_align;
  
  /* The state of the bufferspec value. */
  enum { BUILDING,
         CALLSPEC } state;
  
  /* A null-terminated array of size `nelements' types */
  ffi_type **args;
  
} bufferspec_prototype = {
  0, 0, 0, 0, BUILDING, NULL,
};


static struct callspec {
  struct bufferspec bufferspec;
  
  /* return value offset */
  size_t roffset;
  
  /* The libffi call interface structure.  It would be nice for this member to
     be a value rather than a pointer (to save a layer of indirection) but the
     ffi_closure structure keeps the address of the structure, and the GC can
     move callspec values around.
  */
  ffi_cif *cif;
  
} callspec_prototype = {
  { 0, 0, 0, 0, BUILDING, NULL }, -1, NULL
};


static void finalize_bufferspec(value v)
{
  struct bufferspec *bufferspec = Data_custom_val(v);
  free(bufferspec->args);
  if (bufferspec->state == CALLSPEC) {
    struct callspec *callspec = (struct callspec *)bufferspec;
    free(callspec->cif);
  }
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

/* Compute the size of the buffer needed to hold the pointer array used by
   ffi_call, the arguments and the return value */
static size_t compute_arg_buffer_size(struct bufferspec *bufferspec,
                                      size_t *arg_array_offset)
{
  assert(bufferspec->state == CALLSPEC);
     
  size_t bytes = bufferspec->bytes;
     
  *arg_array_offset = aligned_offset(bytes, ffi_type_pointer.alignment);
  bytes = *arg_array_offset + bufferspec->nelements * sizeof(void *);
     
  return bytes;
}

/* Set the pointers in `arg_array' to the addresses of the argument slots in
   `callbuffer' as indicated by the elements of the ffitype array in the
   bufferspec.  */ 
static void populate_arg_array(struct bufferspec *bufferspec,
                               callbuffer *callbuffer, void **arg_array)
{
  size_t i = 0, offset = 0;
  for (; i < bufferspec->nelements; i++) {
    offset = aligned_offset(offset, bufferspec->args[i]->alignment);
    arg_array[i] = (char *)callbuffer + offset;
    offset += bufferspec->args[i]->size;
  }
}


/* Allocate a new C call specification */
/* allocate_callspec : unit -> callspec */
value ctypes_allocate_callspec(value unit)
{
  return allocate_custom(&bufferspec_custom_ops,
                         sizeof(struct callspec),
                         &callspec_prototype);
}


/* Add an argument to the C call specification */
/* add_argument : bufferspec -> 'a ffitype -> int */
value ctypes_add_argument(value bufferspec_, value argument_)
{
  static const size_t increment_size = 8;

  CAMLparam2(bufferspec_, argument_);
  struct bufferspec *bufferspec = Data_custom_val(bufferspec_);
  ffi_type *argtype = CTYPES_TO_PTR(argument_);

  assert (bufferspec->state == BUILDING);

  /* If there's a possibility that this spec represents an argument list or
     a struct we might pass by value then we have to take care to maintain
     the args, capacity and nelements members. */
  int offset = aligned_offset(bufferspec->bytes, argtype->alignment);
  bufferspec->bytes = offset + argtype->size;

  if (bufferspec->nelements + 2 >= bufferspec->capacity) {
    size_t new_size = ((bufferspec->capacity + increment_size)
                       * sizeof *bufferspec->args);
    bufferspec->args = caml_stat_resize(bufferspec->args, new_size);
    bufferspec->capacity += increment_size;
  }
  bufferspec->args[bufferspec->nelements] = argtype;
  bufferspec->args[bufferspec->nelements + 1] = NULL;
  bufferspec->nelements += 1;
  bufferspec->max_align = argtype->alignment > bufferspec->max_align
    ? argtype->alignment
    : bufferspec->max_align;
  CAMLreturn(Val_int(offset));
}


/* Pass the return type and conclude the specification preparation */
/* prep_callspec : callspec -> 'a ffitype -> unit */
value ctypes_prep_callspec(value callspec_, value rtype)
{
  CAMLparam2(callspec_, rtype);

  struct callspec *callspec = Data_custom_val(callspec_);
  ffi_type *rffitype = CTYPES_TO_PTR(rtype);

  /* Allocate the cif structure */
  callspec->cif = caml_stat_alloc(sizeof *callspec->cif);

  /* Add the (aligned) space needed for the return value */
  callspec->roffset = aligned_offset(callspec->bufferspec.bytes,
                                     rffitype->alignment);
  callspec->bufferspec.bytes = callspec->roffset + rffitype->size;

  /* Allocate an extra word after the return value space to work
     around a bug in libffi which causes it to write past the return
     value space.

     https://github.com/atgreen/libffi/issues/35
  */
  callspec->bufferspec.bytes = aligned_offset(callspec->bufferspec.bytes,
                                              ffi_type_pointer.alignment);
  callspec->bufferspec.bytes += ffi_type_pointer.size;

  ffi_status status = ffi_prep_cif(callspec->cif,
                                   FFI_DEFAULT_ABI,
                                   callspec->bufferspec.nelements,
                                   rffitype,
                                   callspec->bufferspec.args);

  ctypes_check_ffi_status(status);

  callspec->bufferspec.state = CALLSPEC;
  CAMLreturn(Val_unit);
}

/* Call the function specified by `callspec', passing arguments and return
   values in `buffer' */
/* call : raw_pointer -> callspec -> (raw_pointer -> unit) ->
          (raw_pointer -> 'a) -> 'a */
value ctypes_call(value function, value callspec_, value argwriter,
                  value rvreader)
{
  CAMLparam4(function, callspec_, argwriter, rvreader);

  void (*cfunction)(void) = (void (*)(void)) CTYPES_TO_PTR(function);
  struct callspec *callspec = Data_custom_val(callspec_);
  struct bufferspec *bufferspec = (struct bufferspec *)callspec;
  int roffset = callspec->roffset;

  assert(bufferspec->state == CALLSPEC);

  size_t arg_array_offset;
  size_t bytes = compute_arg_buffer_size(bufferspec, &arg_array_offset);

  char *callbuffer = alloca(bytes);
  char *return_slot = callbuffer + roffset;

  populate_arg_array(bufferspec, (struct callbuffer *)callbuffer,
                     (void **)(callbuffer + arg_array_offset));

  caml_callback(argwriter, CTYPES_FROM_PTR(callbuffer));

  ffi_call(callspec->cif,
           cfunction,
           return_slot,
           (void **)(callbuffer + arg_array_offset));

  CAMLreturn(caml_callback(rvreader, CTYPES_FROM_PTR(return_slot)));
}


/* call_errno : string -> raw_pointer -> callspec -> 
               (raw_pointer -> unit) ->
               (raw_pointer -> 'a) -> 'a */
value ctypes_call_errno(value fnname, value function, value callspec_,
                        value argwriter, value rvreader)
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
  int         fnkey;
};

enum boxedfn_tags { Done, Fn };

static void callback_handler(ffi_cif *cif,
                             void *ret,
                             void **args,
                             void *user_data)
{
  CAMLparam0 ();

  CAMLlocal1(boxedfn);
  boxedfn = retrieve_closure(*(int *)user_data);

  int i, arity = cif->nargs;
  for (i = 0; i < arity; i++)
  {
    void *cvalue = args[i];
    assert (Tag_val(boxedfn) == Fn);
    /* unbox and call */
    boxedfn = caml_callback(Field(boxedfn, 0), CTYPES_FROM_PTR(cvalue));
  }

  /* now store the return value */
  assert (Tag_val(boxedfn) == Done);
  caml_callback(Field(boxedfn, 0), CTYPES_FROM_PTR(ret));

  CAMLreturn0;
}


/* Construct a pointer to an OCaml function represented by an identifier */
/* make_function_pointer : callspec -> int -> raw_pointer */
value ctypes_make_function_pointer(value callspec_, value fnid)
{
  CAMLparam2(callspec_, fnid);
  struct callspec *callspec = Data_custom_val(callspec_);

  assert(callspec->bufferspec.state == CALLSPEC);

  void (*code_address)(void) = NULL;

  /* TODO: we need to call ffi_closure_free at some point.  This function
     should return a managed object to which we can attach a finaliser for the
     closure.
  */
  closure *closure = ffi_closure_alloc(sizeof *closure, (void *)&code_address);

  if (closure == NULL) {
    caml_raise_out_of_memory();
  } else {
    closure->fnkey = Int_val(fnid);

    ffi_status status =  ffi_prep_closure_loc
      ((ffi_closure *)closure,
       callspec->cif,
       callback_handler,
       &closure->fnkey,
       (void *)code_address);

    ctypes_check_ffi_status(status);

    CAMLreturn (CTYPES_FROM_PTR((void *)code_address));
  }
}
