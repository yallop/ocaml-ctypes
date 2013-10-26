/*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <errno.h>
#include <stdint.h>
#include <complex.h>

#include <caml/mlvalues.h>

#define ALIGNMENT(T) (offsetof(struct { char c; T t; }, t))
#define FULL_ENTRY(CTOR, T, SIZE, ALIGNMENT) { #CTOR, #T, SIZE, ALIGNMENT }
#define ENTRY(CTOR, T) FULL_ENTRY(CTOR, T, sizeof(T), ALIGNMENT(T))

static struct details {
  const char *constructor;
  const char *name;
  int size, alignment;
} details[] = {
  ENTRY(Char, char),
  ENTRY(Schar, signed char),
  ENTRY(Uchar, unsigned char),
  ENTRY(Short, short),
  ENTRY(Int, int),
  ENTRY(Long, long),
  ENTRY(Llong, long long),
  ENTRY(Ushort, unsigned short),
  ENTRY(Uint, unsigned int),
  ENTRY(Ulong, unsigned long),
  ENTRY(Ullong, unsigned long long),
  ENTRY(Size_t, size_t),
  ENTRY(Int8_t, int8_t),
  ENTRY(Int16_t, int16_t),
  ENTRY(Int32_t, int32_t),
  ENTRY(Int64_t, int64_t),
  ENTRY(Uint8_t, uint8_t),
  ENTRY(Uint16_t, uint16_t),
  ENTRY(Uint32_t, uint32_t),
  ENTRY(Uint64_t, uint64_t),
  FULL_ENTRY(Camlint, camlint, sizeof(intnat), ALIGNMENT(intnat)),
  ENTRY(Nativeint, intnat),
  ENTRY(Float, float),
  ENTRY(Double, double),
  ENTRY(Complex32, float complex),
  ENTRY(Complex64, double complex),
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

value ctypes_make_primitives(value _unit)
{
  printf("open Primitives\n");
  generate_function("sizeof", "int", print_size);
  generate_function("alignment", "int", print_alignment);
  generate_function("name", "string", print_name);
  printf("let pointer_size = %d\n", (int)sizeof(void *));
  printf("let pointer_alignment = %d\n", (int)ALIGNMENT(void *));
  fflush(stdout);
  return Val_unit;
}
