/*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <inttypes.h>
#include <errno.h>
#include <stdint.h>
#include <complex.h>
#include <stdbool.h>

#include <caml/mlvalues.h>

#if __USE_MINGW_ANSI_STDIO && defined(__MINGW64__)
#define REAL_ARCH_INTNAT_PRINTF_FORMAT "ll"
#else
#define REAL_ARCH_INTNAT_PRINTF_FORMAT ARCH_INTNAT_PRINTF_FORMAT
#endif

#define ALIGNMENT(T) (offsetof(struct { char c; T t; }, t))
#define FULL_ENTRY(CTOR, T, FORMAT, SIZE, ALIGNMENT) \
  { #CTOR, #T, FORMAT, SIZE, ALIGNMENT }
#define ENTRY(CTOR, T, FORMAT) \
  FULL_ENTRY(CTOR, T, FORMAT, sizeof(T), ALIGNMENT(T))

static struct details {
  const char *constructor;
  const char *name;
  const char *format_string;
  int size, alignment;
} details[] = {
  ENTRY(Char, char, "%d"),
  ENTRY(Schar, signed char, "%d"),
  ENTRY(Uchar, unsigned char, "%d"),
  ENTRY(Bool, bool, "%d"),
  ENTRY(Short, short, "%hd"),
  ENTRY(Int, int, "%d"),
  ENTRY(Long, long, "%ld"),
  ENTRY(Llong, long long, "%lld"),
  ENTRY(Ushort, unsigned short, "%hu"),
  ENTRY(Uint, unsigned int, "%u"),
  ENTRY(Ulong, unsigned long, "%lu"),
  ENTRY(Ullong, unsigned long long, "%llu"),
  ENTRY(Size_t, size_t, "%zu"),
  ENTRY(Int8_t, int8_t, "%" PRId8),
  ENTRY(Int16_t, int16_t, "%" PRId16),
  ENTRY(Int32_t, int32_t, "%" PRId32),
  ENTRY(Int64_t, int64_t, "%" PRId64),
  ENTRY(Uint8_t, uint8_t, "%" PRIu8),
  ENTRY(Uint16_t, uint16_t, "%" PRIu16),
  ENTRY(Uint32_t, uint32_t, "%" PRIu32),
  ENTRY(Uint64_t, uint64_t, "%" PRIu64),
  FULL_ENTRY(Camlint, camlint,
             "%" REAL_ARCH_INTNAT_PRINTF_FORMAT "d",
             sizeof(intnat), ALIGNMENT(intnat)),
  ENTRY(Nativeint, intnat, "%" REAL_ARCH_INTNAT_PRINTF_FORMAT "d"),
  ENTRY(Float, float, "%.12g"),
  ENTRY(Double, double, "%.12g"),
  ENTRY(Complex32, float complex, NULL),
  ENTRY(Complex64, double complex, NULL),
};

void generate_function(char *name, char *type,
                       void (*cse)(struct details*))
{
  int i;
  printf("let %s : type a. a prim -> %s = function\n", name, type);
  for (i = 0; i < sizeof details / sizeof *details; i++) {
    printf(" | %s -> ", details[i].constructor);
    cse(&details[i]);
    printf("\n");
  }
}

void print_size(struct details *d)
{
  printf("%d", d->size);
}

void print_alignment(struct details *d)
{
  printf("%d", d->alignment);
}

void print_name(struct details *d)
{
  printf("\"%s\"", d->name);
}

void print_format_string(struct details *d)
{
  if (d->format_string != NULL) {
    printf("Some \"%s\"", d->format_string);
  }
  else {
    printf("None");
  }
}

value ctypes_make_primitives(value _unit)
{
  printf("open Primitives\n");
  generate_function("sizeof", "int", print_size);
  generate_function("alignment", "int", print_alignment);
  generate_function("name", "string", print_name);
  generate_function("format_string", "string option", print_format_string);
  printf("let pointer_size = %d\n", (int)sizeof(void *));
  printf("let pointer_alignment = %d\n", (int)ALIGNMENT(void *));
  fflush(stdout);
  return Val_unit;
}
