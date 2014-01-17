/*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 */

#ifndef TEST_FUNCTIONS_H
#define TEST_FUNCTIONS_H

#include <inttypes.h>
#include <stdlib.h>
#include <complex.h>

#include <caml/mlvalues.h>

typedef int intfun(int, int);
extern int higher_order_1(intfun *, int, int);
typedef int acceptor(intfun *, int, int);
extern int higher_order_3(acceptor *, intfun *, int, int);
typedef int vintfun(int);
extern int higher_order_simplest(vintfun *);
extern intfun *returning_funptr(int);
extern int accepting_possibly_null_funptr(intfun *, int, int);
extern int global;
extern int *return_global_address(void);
extern double float_pointer_callback(void (*)(double *), double);
extern int write_through_callback(int (*)(int *));
extern int write_through_callback_pointer_pointer(int (*)(int **, int *));
extern int is_null(void *);
extern int callback_returns_funptr(vintfun *(*)(int), int);
extern int *pass_pointer_through(int *, int *, int);
struct simple {
  int i;
  double f;
  struct simple *self;
};

extern int accept_struct(struct simple);
extern struct simple return_struct(void);
union padded;
extern int64_t sum_union_components(union padded *, size_t);

extern void concat_strings(const char **, int, char *);

union number {
  int i;
  double d;
};

struct tagged {
  char tag;
  union number num;
};

extern double accepts_pointer_to_array_of_structs(struct tagged(*)[5]);
#define GLOBAL_STRING "global string"
struct global_struct {
  size_t len;
  const char str[sizeof GLOBAL_STRING];
};

extern struct global_struct global_struct;

struct animal;
struct chorse;
extern int check_name(struct animal *, char *);
extern char *chorse_colour(struct chorse *);
extern char *chorse_say(struct animal *);
extern char *chorse_identify(struct animal *);
extern struct chorse *new_chorse(int);
extern int accept_pointers(float *,
                           double *,
                           short *,
                           int *,
                           long *,
                           long long *,
                           intnat *,
                           int8_t *,
                           int16_t *,
                           int32_t *,
                           int64_t *,
                           uint8_t *,
                           uint16_t *,
                           uint32_t *,
                           uint64_t *,
                           size_t *,
                           unsigned short *,
                           unsigned *,
                           unsigned long *,
                           unsigned long long *);
int accept_pointers_to_pointers(int *, int **, int ***, int ****);
intfun **returning_pointer_to_function_pointer(void);
int accepting_pointer_to_function_pointer(intfun **);
typedef int pintfun1(int *, int *);
int passing_pointers_to_callback(pintfun1 *);
typedef int *pintfun2(int, int);
int accepting_pointer_from_callback(pintfun2 *);
signed char retrieve_SCHAR_MIN(void);
signed char retrieve_SCHAR_MAX(void);
unsigned char retrieve_UCHAR_MAX(void);
char retrieve_CHAR_MIN(void);
char retrieve_CHAR_MAX(void);
short retrieve_SHRT_MIN(void);
short retrieve_SHRT_MAX(void);
unsigned short retrieve_USHRT_MAX(void);
int retrieve_INT_MIN(void);
int retrieve_INT_MAX(void);
unsigned int retrieve_UINT_MAX(void);
long retrieve_LONG_MAX(void);
long retrieve_LONG_MIN(void);
long retrieve_nLONG_MAX(void);
long retrieve_nLONG_MIN(void);
unsigned long retrieve_ULONG_MAX(void);
long long retrieve_LLONG_MAX(void);
long long retrieve_LLONG_MIN(void);
unsigned long long retrieve_ULLONG_MAX(void);
int8_t retrieve_INT8_MIN(void);
int16_t retrieve_INT16_MIN(void);
int32_t retrieve_INT32_MIN(void);
int64_t retrieve_INT64_MIN(void);
int8_t retrieve_INT8_MAX(void);
int16_t retrieve_INT16_MAX(void);
int32_t retrieve_INT32_MAX(void);
int64_t retrieve_INT64_MAX(void);
uint8_t retrieve_UINT8_MAX(void);
uint16_t retrieve_UINT16_MAX(void);
uint32_t retrieve_UINT32_MAX(void);
uint64_t retrieve_UINT64_MAX(void);
size_t retrieve_SIZE_MAX(void);
float retrieve_FLT_MIN(void);
float retrieve_FLT_MAX(void);
double retrieve_DBL_MIN(void);
double retrieve_DBL_MAX(void);
void add_complexd(double complex *, double complex *, double complex *);
void mul_complexd(double complex *, double complex *, double complex *);
void add_complexf(float complex *, float complex *, float complex *);
void mul_complexf(float complex *, float complex *, float complex *);
void store_callback(int (*callback)(int));
int invoke_stored_callback(int);
vintfun *return_callback(vintfun *);
struct one_int { int i; };
struct one_int return_struct_by_value(void);
void matrix_mul(int, int, int, double *, double *, double *);
double *matrix_transpose(int, int, double *);
int (*plus_callback)(int);
int sum_range_with_plus_callback(int, int);

#endif /* TEST_FUNCTIONS_H */
