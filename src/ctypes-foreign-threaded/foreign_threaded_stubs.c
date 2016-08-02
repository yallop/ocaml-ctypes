/*
 * Copyright (c) 2016 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 */

#include <caml/mlvalues.h>
#include <caml/threads.h>

#ifdef _WIN32
value ctypes_setup_thread_registration(value _)
{
  /* Don't override the hook on systems without pthreads. */
  return Val_unit;
}
#else
#include <pthread.h>

extern int (*ctypes_thread_register)(void);

static pthread_key_t cleanup_key;

static void ctypes_thread_unregister(void* _)
{
  caml_c_thread_unregister();
  pthread_setspecific(cleanup_key, NULL);
}

static int ctypes_thread_actually_register(void)
{
  int rv = caml_c_thread_register();

  if (rv != 0) {
    /* Register a destructor function for a TLS key that unregisters
       this thread from the OCaml runtime when the thread exits. */

    /* Assumption: caml_c_thread_unregister is not called in this
       thread, except by the destructor, so caml_c_thread_register()
       will always succeed.  Consequently, there is no need to protect
       the TLS-creation code with pthread_once.  (And at worst, if the
       assumption is violated then caml_c_thread_unregister will be
       called multiple times, which is harmless.) */ 
    pthread_key_create(&cleanup_key, ctypes_thread_unregister);
    pthread_setspecific(cleanup_key, &cleanup_key);
  }

  return rv;
}

value ctypes_setup_thread_registration(value _)
{
  ctypes_thread_register = ctypes_thread_actually_register;
  return Val_unit;
}
#endif
