/*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 */

#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdint.h>
#include <limits.h>
#include <inttypes.h>
#include <float.h>
#include <assert.h>
#include <string.h>
#include <complex.h>

#if defined _WIN32 && !defined __CYGWIN__
#include <windows.h>
#else
#include <semaphore.h>
#endif

#include "test_functions.h"

static int add(int x, int y) { return x + y; }
static int times(int x, int y) { return x * y; }

int higher_order_1(intfun *callback, int x, int y)
{
  return callback(x, y) == x + y;
}

int higher_order_3(acceptor *callback, intfun *fn, int x, int y)
{
  return callback(fn, x, y);
}

int higher_order_simplest(vintfun *callback)
{
  return callback(22);
}

intfun *returning_funptr(int v)
{
  switch (v)
  {
  case 0: return add;
  case 1: return times;
  default: return NULL;
  }
}

int accepting_possibly_null_funptr(intfun *f, int x, int y)
{
  return f != NULL ? f(x, y) : -1;
}

int global = 100;

int *return_global_address(void)
{
  return &global;
}

double float_pointer_callback(void (*f)(double *), double v)
{
  f(&v);
  return v * 2.0;
}

int write_through_callback(int (*f)(int *))
{
  int x = 42;
  return f(&x) + x;
}

int write_through_callback_pointer_pointer(int (*f)(int **, int *))
{
  int x = 10, y = 20;
  int *p =&x;
  return f(&p, &y) + *p + x + y;
}

int is_null(void *p)
{
  return p == NULL;
}

int callback_returns_funptr(vintfun *(*callback)(int), int x)
{
  vintfun *v1 = callback(x);
  vintfun *v2 = callback(x + 1);

  return v1(10) + v2(20);
}


int *pass_pointer_through(int *a, int *b, int i)
{
  return (i >= 0) ? a : b;
}

int accept_struct(struct simple simple)
{
  return simple.i + (int)simple.f + (simple.self == NULL ? 1 : 0);
}

struct simple return_struct(void)
{
  struct simple *t = malloc(sizeof *t);
  t->i = 10;
  t->f = 12.5;
  t->self = t;

  struct simple s = {
    20,
    35.0,
    t
  };

  return s;
}

int64_t sum_union_components(union padded *padded, size_t len)
{
  size_t i;
  int64_t acc = 0;
  for (i = 0; i < len; i++) {
    acc += padded[i].i;
  }
  return acc;
}

union padded add_unions(union padded l, union padded r)
{
  union padded result, args[] = { l, r };
  result.i = sum_union_components(args, sizeof args / sizeof *args);
  return result;
}


void concat_strings(const char **sv, int sc, char *buffer)
{
  int i = 0;
  for (; i < sc; i++) {
    const char *s = sv[i];
    while (*s) {
      *buffer++ = *s++;
    }
  }
  *buffer = '\0';
}


struct tagged add_tagged_numbers(struct tagged l, struct tagged r)
{
  union number n;
  struct tagged result = { 'd', n };
  switch (l.tag) {
  case 'i':
    switch (r.tag) {
    case 'i':
      result.num.d = l.num.i + r.num.i;
      return result;
    case 'd':
      result.num.d = l.num.i + r.num.d;
      return result;
    default: assert(0);
    }
  case 'd':
    switch (r.tag) {
    case 'i':
      result.num.d = l.num.d + r.num.i;
      return result;
    case 'd':
      result.num.d = l.num.d + r.num.d;
      return result;
    default: assert(0);
    }
  default: assert(0);
  }
}


double accepts_pointer_to_array_of_structs(struct tagged(*arr)[5])
{
  double sum = 0.0;
  int i = 0;
  struct tagged *s = &(*arr[0]);
  for (; i < 5; i++) {
    switch (s[i].tag) {
    case 'i': {
      sum += s[i].num.i;
      break;
    }
    case 'd': {
      sum += s[i].num.d;
      break;
    }
    default: assert(0);
    }
  }
  return sum;
}

struct global_struct global_struct = { sizeof GLOBAL_STRING - 1, GLOBAL_STRING };


struct triple add_triples(struct triple l, struct triple r)
{
  int i = 0;
  struct triple result;
  for (; i < 3; i++) {
    result.elements[i] = l.elements[i] + r.elements[i];
  }
  return result;
}


/* OO-style example */
struct animal_methods;
struct animal {
  struct animal_methods *vtable;
};
struct animal_methods {
  char *(*say)(struct animal *);
  char *(*identify)(struct animal *);
};

int check_name(struct animal *a, char *name)
{
  return strcmp(a->vtable->identify(a), name) == 0;
}

enum colour { white, red, black, pale };

struct chorse_methods;

struct chorse {
  struct chorse_methods *vtable;
  enum colour colour;
};

struct chorse_methods {
  struct animal_methods base;
  char *(*colour)(struct chorse *);
};

char *chorse_colour(struct chorse *chorse)
{
  switch (chorse->colour) {
  case white : return "white";
  case red   : return "red";
  case black : return "black";
  case pale  : return "pale";
  default: assert(0);
  }
}

char *chorse_say(struct animal *c)
{
  return "neigh";
}

char *chorse_identify(struct animal *a)
{
  static char buffer[30]; /* static allocation is adequate for the test */
  sprintf(buffer, "%s horse", chorse_colour((struct chorse *)a));
  return buffer;
}

static struct chorse_methods chorse_vtable = {
  {
    chorse_say,
    chorse_identify,
  },
  chorse_colour,
};

struct chorse *new_chorse(int colour)
{
  struct chorse *h = malloc(sizeof *h);
  h->vtable = &chorse_vtable;
  h->colour = (enum colour)colour;
  return h;
}
/* (End of OO-style example) */

int accept_pointers(float *float_p,
                    double *double_p,
                    short *short_p,
                    int *int_p,
                    long *long_p,
                    long long *llong_p,
                    intnat *nativeint_p,
                    int8_t *int8_t_p,
                    int16_t *int16_t_p,
                    int32_t *int32_t_p,
                    int64_t *int64_t_p,
                    uint8_t *uint8_t_p,
                    uint16_t *uint16_t_p,
                    uint32_t *uint32_t_p,
                    uint64_t *uint64_t_p,
                    size_t *size_t_p,
                    unsigned short *ushort_p,
                    unsigned *uint_p,
                    unsigned long *ulong_p,
                    unsigned long long *ullong_p)
{
  return (*float_p
          + *double_p
          + *short_p
          + *int_p
          + *long_p
          + *llong_p
          + *nativeint_p
          + *int8_t_p
          + *int16_t_p
          + *int32_t_p
          + *int64_t_p
          + *uint8_t_p
          + *uint16_t_p
          + *uint32_t_p
          + *uint64_t_p
          + *size_t_p
          + *ushort_p
          + *uint_p
          + *ulong_p
          + *ullong_p);
}

int accept_pointers_to_pointers(int *p, int **pp, int ***ppp, int ****pppp)
{
  return *p + **pp + ***ppp + ****pppp;
}

intfun **returning_pointer_to_function_pointer(void)
{
  static intfun *f = times;
  return &f;
}

int accepting_pointer_to_function_pointer(intfun **pfp)
{
  return (*pfp)(20, 4);
}

int passing_pointers_to_callback(pintfun1 *f)
{
  int x = 3, y = 4;
  return f(&x, &y);
}

int accepting_pointer_from_callback(pintfun2 *f)
{
  int *p = f(7, 8);
  int q = *p;
  *p = 12;
  return q;
}


signed char retrieve_SCHAR_MIN(void) { return SCHAR_MIN; }
signed char retrieve_SCHAR_MAX(void) { return SCHAR_MAX; }
unsigned char retrieve_UCHAR_MAX(void) { return UCHAR_MAX; }
char retrieve_CHAR_MIN(void) { return CHAR_MIN; }
char retrieve_CHAR_MAX(void) { return CHAR_MAX; }
short retrieve_SHRT_MIN(void) { return SHRT_MIN; }
short retrieve_SHRT_MAX(void) { return SHRT_MAX; }
unsigned short retrieve_USHRT_MAX(void) { return USHRT_MAX; }
int retrieve_INT_MIN(void) { return INT_MIN; }
int retrieve_INT_MAX(void) { return INT_MAX; }
unsigned int retrieve_UINT_MAX(void) { return UINT_MAX; }
long retrieve_LONG_MAX(void) { return LONG_MAX; }
long retrieve_LONG_MIN(void) { return LONG_MIN; }
unsigned long retrieve_ULONG_MAX(void) { return ULONG_MAX; }
long long retrieve_LLONG_MAX(void) { return LLONG_MAX; }
long long retrieve_LLONG_MIN(void) { return LLONG_MIN; }
unsigned long long retrieve_ULLONG_MAX(void) { return ULLONG_MAX; }
int8_t retrieve_INT8_MIN(void) { return INT8_MIN; }
int16_t retrieve_INT16_MIN(void) { return INT16_MIN; }
int32_t retrieve_INT32_MIN(void) { return INT32_MIN; }
int64_t retrieve_INT64_MIN(void) { return INT64_MIN; }
int8_t retrieve_INT8_MAX(void) { return INT8_MAX; }
int16_t retrieve_INT16_MAX(void) { return INT16_MAX; }
int32_t retrieve_INT32_MAX(void) { return INT32_MAX; }
int64_t retrieve_INT64_MAX(void) { return INT64_MAX; }
uint8_t retrieve_UINT8_MAX(void) { return UINT8_MAX; }
uint16_t retrieve_UINT16_MAX(void) { return UINT16_MAX; }
uint32_t retrieve_UINT32_MAX(void) { return UINT32_MAX; }
uint64_t retrieve_UINT64_MAX(void) { return UINT64_MAX; }
size_t retrieve_SIZE_MAX(void) { return SIZE_MAX; }
float retrieve_FLT_MIN(void) { return FLT_MIN; }
float retrieve_FLT_MAX(void) { return FLT_MAX; }
double retrieve_DBL_MIN(void) { return DBL_MIN; }
double retrieve_DBL_MAX(void) { return DBL_MAX; }

void add_complexd(double complex *l, double complex *r, double complex *out)
{
  *out = *l + *r;
}

void mul_complexd(double complex *l, double complex *r, double complex *out)
{
  *out = *l * *r;
}

void add_complexf(float complex *l, float complex *r, float complex *out)
{
  *out = *l + *r;
}

void mul_complexf(float complex *l, float complex *r, float complex *out)
{
  *out = *l * *r;
}

double complex add_complexd_val(double complex l, double complex r)
{
  return l + r;
}

double complex mul_complexd_val(double complex l, double complex r)
{
  return l * r;
}

float complex add_complexf_val(float complex l, float complex r)
{
  return l + r;
}

float complex mul_complexf_val(float complex l, float complex r)
{
  return l * r;
}

static int (*global_stored_callback)(int) = NULL;

void store_callback(int (*callback)(int))
{
  global_stored_callback = callback;
}

int invoke_stored_callback(int x)
{
  return global_stored_callback(x);
}

vintfun *return_callback(vintfun *callback)
{
  return callback;
}

struct one_int return_struct_by_value(void)
{
  struct one_int v = { 3 };
  return v;
}

/* naive matrix operations */
void matrix_mul(int lrows, int lcols, int rcols,
                double *l, double *r, double *prod)
{
  int i, j, k;
  for (i = 0; i < lrows; i++) {
    for (j = 0; j < rcols; j++) {
      prod[i * rcols + j] = 0.0;
      for (k = 0; k < lcols; k++) {
        prod[i * rcols + j] += l[i * lcols + k] * r[k * rcols + j];
      }
    }
  }
}

double *matrix_transpose(int rows, int cols, double *matrix)
{
  int i, j;
  double *rv = malloc(rows * cols * sizeof *rv);

  for (i = 0; i < rows; i++)
    for (j = 0; j < cols; j++)
      rv[j * rows + i] = matrix[i * cols + j];

  return rv;
}

int (*plus_callback)(int) = NULL;

/* Sum the range [a, b] */
int sum_range_with_plus_callback(int a, int b)
{
  int sum = 0, i = 0;
  for (i = a; i <= b; i++) {
    sum += i;
  }
  return sum;
}

static callback_t *registered_callback = NULL;

void register_callback(callback_t *callback)
{
  registered_callback = callback;
}

void call_registered_callback(int times, int starting_value)
{
  int i;
  for (i = 0; i < times; i++) {
    int result = registered_callback();
    assert (result == starting_value++);
  }
}

#if defined _WIN32 && !defined __CYGWIN__
#define sem_t HANDLE
#define sem_init(sem, sem_attr1, sem_init_value)        \
  (void)((*sem = CreateSemaphore(NULL,0,32768,NULL))==NULL)
#define sem_wait(sem) \
  (void)(WAIT_OBJECT_0 != WaitForSingleObject(*sem,INFINITE))
#define sem_post(sem) (void)ReleaseSemaphore(*sem,1,NULL)
#endif

static sem_t semaphore1;
static sem_t semaphore2;

void initialize_waiters(void)
{
  sem_init(&semaphore1, 0, -1);
  sem_init(&semaphore2, 0, -1);
}

void post1_wait2(void)
{
  sem_post(&semaphore1);
  sem_wait(&semaphore2);
}

void post2_wait1(void)
{
  sem_post(&semaphore2);
  sem_wait(&semaphore1);
}

size_t sizeof_s1(void) { return sizeof(struct s1); }
size_t alignmentof_s1(void) { return offsetof(struct { char c; struct s1 x; }, x); }
size_t offsetof_x1(void) { return offsetof(struct s1, x1); }
size_t offsetof_x2(void) { return offsetof(struct s1, x2); }
size_t offsetof_x3(void) { return offsetof(struct s1, x3); }
size_t offsetof_x4(void) { return offsetof(struct s1, x4); }
size_t sizeof_s2(void) { return sizeof(struct s2); }
size_t alignmentof_s2(void) { return offsetof(struct { char c; struct s2 x; }, x); }
size_t offsetof_y1(void) { return offsetof(struct s2, y1); }
size_t offsetof_y2(void) { return offsetof(struct s2, y2); }
size_t offsetof_y3(void) { return offsetof(struct s2, y3); }
size_t offsetof_y4(void) { return offsetof(struct s2, y4); }
size_t sizeof_s3(void) { return sizeof(struct s3); }
size_t alignmentof_s3(void) { return offsetof(struct { char c; struct s3 x; }, x); }
size_t offsetof_z1(void) { return offsetof(struct s3, z1); }
size_t offsetof_z2(void) { return offsetof(struct s3, z2); }
size_t sizeof_s4(void) { return sizeof(struct s4); }
size_t alignmentof_s4(void) { return offsetof(struct { char c; struct s4 x; }, x); }
size_t offsetof_z3(void) { return offsetof(struct s4, z3); }
size_t offsetof_z4(void) { return offsetof(struct s4, z4); }
size_t sizeof_s6(void) { return sizeof(s6); }
size_t alignmentof_s6(void) { return offsetof(struct { char c; s6 x; }, x); }
size_t offsetof_v1(void) { return offsetof(s6, v1); }
size_t offsetof_v2(void) { return offsetof(s6, v2); }

size_t sizeof_u1(void) { return sizeof(union u1); }
size_t alignmentof_u1(void) { return offsetof (struct { char c; union u1 x; }, x); }
size_t sizeof_u2(void) { return sizeof(u2); }
size_t alignmentof_u2(void) { return offsetof (struct { char c; u2 x; }, x); }

bool bool_and(bool l, bool r)
{
  return l && r;
}

int call_s5(struct s1 *s1, struct s5 *s5)
{
  return s5->w1(s1);
}

enum signed_enum classify_integer(int x)
{
  return (x < 0) ? minus_one : plus_one;
}

enum signed_enum out_of_range(void)
{
  return (enum signed_enum)2;
}

enum fruit next_fruit(enum fruit f)
{
  switch (f)
  {
  case Orange: return Apple;
  case Apple: return Banana;
  case Banana: return Pear;
  case Pear: return Orange;
  default: assert(0);
  }
}

int32_t sum_int_array(int32_t *arr, size_t len)
{
  int32_t sum = 0;
  size_t i = 0;
  for (; i < len; i++) {
    sum += arr[i];
  }
  return sum;
}

void *global_ocaml_value = NULL;

void save_ocaml_value(void *p)
{
  global_ocaml_value = p;
}
void *retrieve_ocaml_value(void)
{
  return global_ocaml_value;
}

int sixargs(int x1, int x2, int x3, int x4, int x5, int x6)
{
  return x1 + x2 + x3 + x4 + x5 + x6;
}

int return_10(void)
{
  return 10;
}

int callback_returns_char_a(char (*f)(void))
{
  return f() == 'a' ? 1 : 0;
}
