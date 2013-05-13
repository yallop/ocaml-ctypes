#ifndef MANAGED_BUFFER_STUBS_H
#define MANAGED_BUFFER_STUBS_H

#include <caml/mlvalues.h>

/* allocate : int -> managed_buffer */
extern value ctypes_allocate(value size);

/* block_address : managed_buffer -> immediate_pointer */
extern value ctypes_block_address(value managed_buffer);

#endif /* MANAGED_BUFFER_STUBS_H */
