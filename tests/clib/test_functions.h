/*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 */

#ifndef TEST_FUNCTIONS_H
#define TEST_FUNCTIONS_H

#include <inttypes.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdint.h>

#include <caml/mlvalues.h>
#ifdef _MSC_VER
# include <complex.h> /* see https://docs.microsoft.com/en-us/cpp/c-runtime-library/complex-math-support?view=msvc-160 */
# ifdef CTYPES_DLL_EXTERN
#  define DLL_EXTERN __declspec(dllexport)
# else
#  define DLL_EXTERN __declspec(dllimport)
# endif
#else
# define DLL_EXTERN extern
#endif

#ifdef _MSC_VER
typedef _Lcomplex            ctypes_complex_long_double;
typedef _Dcomplex            ctypes_complex_double;
typedef _Fcomplex            ctypes_complex_float;
#else
typedef long double _Complex ctypes_complex_long_double;
typedef double _Complex      ctypes_complex_double;
typedef float _Complex       ctypes_complex_float;
#endif

typedef int intfun(int, int);
DLL_EXTERN int higher_order_1(intfun *, int, int);
typedef int acceptor(intfun *, int, int);
DLL_EXTERN int higher_order_3(acceptor *, intfun *, int, int);
typedef int vintfun(int);
DLL_EXTERN int higher_order_simplest(vintfun *);
DLL_EXTERN intfun *returning_funptr(int);
DLL_EXTERN int accepting_possibly_null_funptr(intfun *, int, int);
DLL_EXTERN int global;
DLL_EXTERN int *return_global_address(void);
DLL_EXTERN double float_pointer_callback(void (*)(double *), double);
DLL_EXTERN int write_through_callback(int (*)(int *));
DLL_EXTERN int write_through_callback_pointer_pointer(int (*)(int **, int *));
DLL_EXTERN int is_null(void *);
DLL_EXTERN int callback_returns_funptr(vintfun *(*)(int), int);
DLL_EXTERN int *pass_pointer_through(int *, int *, int);
struct simple {
  int i;
  double f;
  struct simple *self;
};

DLL_EXTERN int accept_struct(struct simple);
DLL_EXTERN struct simple return_struct(void);
union padded {
  int64_t i;
  char    a[sizeof(int64_t) + 1];
};
DLL_EXTERN int64_t sum_union_components(union padded *, size_t);
DLL_EXTERN union padded add_unions(union padded, union padded);

DLL_EXTERN void concat_strings(const char **, int, char *);

union number {
  int i;
  double d;
};

struct tagged {
  char tag;
  union number num;
};

DLL_EXTERN struct tagged add_tagged_numbers(struct tagged, struct tagged);

DLL_EXTERN double accepts_pointer_to_array_of_structs(struct tagged(*)[5]);
#define GLOBAL_STRING "global string"
struct global_struct {
  size_t len;
  const char str[sizeof GLOBAL_STRING];
};

DLL_EXTERN struct global_struct global_struct;
struct triple {
  double elements[3];
};
DLL_EXTERN struct triple add_triples(struct triple, struct triple);
struct animal;
struct chorse;
DLL_EXTERN int check_name(struct animal *, char *);
DLL_EXTERN char *chorse_colour(struct chorse *);
DLL_EXTERN char *chorse_say(struct animal *);
DLL_EXTERN char *chorse_identify(struct animal *);
DLL_EXTERN struct chorse *new_chorse(int);
DLL_EXTERN int accept_pointers(float *,
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
DLL_EXTERN int accept_pointers_to_pointers(int *, int **, int ***, int ****);
DLL_EXTERN intfun **returning_pointer_to_function_pointer(void);
DLL_EXTERN int accepting_pointer_to_function_pointer(intfun **);
typedef int pintfun1(int *, int *);
DLL_EXTERN int passing_pointers_to_callback(pintfun1 *);
typedef int *pintfun2(int, int);
DLL_EXTERN int accepting_pointer_from_callback(pintfun2 *);
DLL_EXTERN signed char retrieve_SCHAR_MIN(void);
DLL_EXTERN signed char retrieve_SCHAR_MAX(void);
DLL_EXTERN unsigned char retrieve_UCHAR_MAX(void);
DLL_EXTERN char retrieve_CHAR_MIN(void);
DLL_EXTERN char retrieve_CHAR_MAX(void);
DLL_EXTERN short retrieve_SHRT_MIN(void);
DLL_EXTERN short retrieve_SHRT_MAX(void);
DLL_EXTERN unsigned short retrieve_USHRT_MAX(void);
DLL_EXTERN int retrieve_INT_MIN(void);
DLL_EXTERN int retrieve_INT_MAX(void);
DLL_EXTERN unsigned int retrieve_UINT_MAX(void);
DLL_EXTERN long retrieve_LONG_MAX(void);
DLL_EXTERN long retrieve_LONG_MIN(void);
DLL_EXTERN unsigned long retrieve_ULONG_MAX(void);
DLL_EXTERN long long retrieve_LLONG_MAX(void);
DLL_EXTERN long long retrieve_LLONG_MIN(void);
DLL_EXTERN unsigned long long retrieve_ULLONG_MAX(void);
DLL_EXTERN int8_t retrieve_INT8_MIN(void);
DLL_EXTERN int16_t retrieve_INT16_MIN(void);
DLL_EXTERN int32_t retrieve_INT32_MIN(void);
DLL_EXTERN int64_t retrieve_INT64_MIN(void);
DLL_EXTERN int8_t retrieve_INT8_MAX(void);
DLL_EXTERN int16_t retrieve_INT16_MAX(void);
DLL_EXTERN int32_t retrieve_INT32_MAX(void);
DLL_EXTERN int64_t retrieve_INT64_MAX(void);
DLL_EXTERN uint8_t retrieve_UINT8_MAX(void);
DLL_EXTERN uint16_t retrieve_UINT16_MAX(void);
DLL_EXTERN uint32_t retrieve_UINT32_MAX(void);
DLL_EXTERN uint64_t retrieve_UINT64_MAX(void);
DLL_EXTERN size_t retrieve_SIZE_MAX(void);
DLL_EXTERN float retrieve_FLT_MIN(void);
DLL_EXTERN float retrieve_FLT_MAX(void);
DLL_EXTERN double retrieve_DBL_MIN(void);
DLL_EXTERN double retrieve_DBL_MAX(void);
DLL_EXTERN void add_complexd(ctypes_complex_double *, ctypes_complex_double *, ctypes_complex_double *);
DLL_EXTERN void mul_complexd(ctypes_complex_double *, ctypes_complex_double *, ctypes_complex_double *);
DLL_EXTERN void inv_complexd(ctypes_complex_double *, ctypes_complex_double *);
DLL_EXTERN void rotdist_complexd(ctypes_complex_double *, double *, double *);
DLL_EXTERN void add_complexld(ctypes_complex_long_double *, ctypes_complex_long_double *, ctypes_complex_long_double *);
DLL_EXTERN void mul_complexld(ctypes_complex_long_double *, ctypes_complex_long_double *, ctypes_complex_long_double *);
DLL_EXTERN void inv_complexld(ctypes_complex_long_double *, ctypes_complex_long_double *);
DLL_EXTERN void rotdist_complexld(ctypes_complex_long_double *, long double *, long double *);
DLL_EXTERN void add_complexf(ctypes_complex_float *, ctypes_complex_float *, ctypes_complex_float *);
DLL_EXTERN void mul_complexf(ctypes_complex_float *, ctypes_complex_float *, ctypes_complex_float *);
DLL_EXTERN void inv_complexf(ctypes_complex_float *, ctypes_complex_float *);
DLL_EXTERN void rotdist_complexf(ctypes_complex_float *, float *, float *);
DLL_EXTERN ctypes_complex_double add_complexd_val(ctypes_complex_double, ctypes_complex_double);
DLL_EXTERN ctypes_complex_double mul_complexd_val(ctypes_complex_double, ctypes_complex_double);
DLL_EXTERN ctypes_complex_double inv_complexd_val(ctypes_complex_double);
DLL_EXTERN double rotdist_complexd_val(ctypes_complex_double, double);
DLL_EXTERN ctypes_complex_long_double add_complexld_val(ctypes_complex_long_double, ctypes_complex_long_double);
DLL_EXTERN ctypes_complex_long_double mul_complexld_val(ctypes_complex_long_double, ctypes_complex_long_double);
DLL_EXTERN ctypes_complex_long_double inv_complexld_val(ctypes_complex_long_double);
DLL_EXTERN long double rotdist_complexld_val(ctypes_complex_long_double, long double);
DLL_EXTERN ctypes_complex_float add_complexf_val(ctypes_complex_float, ctypes_complex_float);
DLL_EXTERN ctypes_complex_float mul_complexf_val(ctypes_complex_float, ctypes_complex_float);
DLL_EXTERN ctypes_complex_float inv_complexf_val(ctypes_complex_float);
DLL_EXTERN float rotdist_complexf_val(ctypes_complex_float, float);
DLL_EXTERN void store_callback(int (*callback)(int));
DLL_EXTERN int invoke_stored_callback(int);
DLL_EXTERN vintfun *return_callback(vintfun *);
struct one_int { int i; };
DLL_EXTERN struct one_int return_struct_by_value(void);
DLL_EXTERN void matrix_mul(int, int, int, double *, double *, double *);
DLL_EXTERN double *matrix_transpose(int, int, double *);
DLL_EXTERN int (*plus_callback)(int);
DLL_EXTERN int sum_range_with_plus_callback(int, int);
typedef int callback_t(void);
DLL_EXTERN void register_callback(callback_t *);
DLL_EXTERN void call_registered_callback(int, int);
DLL_EXTERN void initialize_waiters(void);
DLL_EXTERN void post1_wait2(void);
DLL_EXTERN void post2_wait1(void);

struct s1 { int x1, x2, x3, x4; };
struct s2 { int y1, y2, y3, y4; };
struct s3 { int z1; struct s3 *z2; };
struct s4 { struct s3 z3; struct s3 *z4; };
struct s5 { int (*w1)(struct s1 *); };
typedef struct { int v1; float v2; } s6;

DLL_EXTERN size_t sizeof_s1(void);
DLL_EXTERN size_t alignmentof_s1(void);
DLL_EXTERN size_t offsetof_x1(void);
DLL_EXTERN size_t offsetof_x2(void);
DLL_EXTERN size_t offsetof_x3(void);
DLL_EXTERN size_t offsetof_x4(void);
DLL_EXTERN size_t sizeof_s2(void);
DLL_EXTERN size_t alignmentof_s2(void);
DLL_EXTERN size_t offsetof_y1(void);
DLL_EXTERN size_t offsetof_y2(void);
DLL_EXTERN size_t offsetof_y3(void);
DLL_EXTERN size_t offsetof_y4(void);

DLL_EXTERN size_t sizeof_s3(void);
DLL_EXTERN size_t alignmentof_s3(void);
DLL_EXTERN size_t offsetof_z1(void);
DLL_EXTERN size_t offsetof_z2(void);

DLL_EXTERN size_t sizeof_s4(void);
DLL_EXTERN size_t alignmentof_s4(void);
DLL_EXTERN size_t offsetof_z3(void);
DLL_EXTERN size_t offsetof_z4(void);

DLL_EXTERN size_t sizeof_s6(void);
DLL_EXTERN size_t alignmentof_s6(void);
DLL_EXTERN size_t offsetof_v1(void);
DLL_EXTERN size_t offsetof_v2(void);

union u1 { char x1; float x2; double x3; char x4[13]; };
typedef union { int t1; float t2; } u2;

DLL_EXTERN size_t sizeof_u1(void);
DLL_EXTERN size_t alignmentof_u1(void);

DLL_EXTERN size_t sizeof_u2(void);
DLL_EXTERN size_t alignmentof_u2(void);

DLL_EXTERN bool bool_and(bool, bool);
DLL_EXTERN int call_s5(struct s1 *, struct s5 *);

enum letter { A, B, C = 10, D };

enum fruit { Orange, Apple, Banana, Pear };
enum bears { Edward, Winnie, Paddington };
enum signed_enum { minus_one = -1, plus_one = 1 };

DLL_EXTERN enum fruit next_fruit(enum fruit);
DLL_EXTERN enum signed_enum classify_integer(int);
DLL_EXTERN enum signed_enum out_of_range(void);

struct fruit_cell {
  enum fruit frt;
  struct fruit_cell *next;
};

typedef enum letter letter_t;
typedef enum bears bears_t;

DLL_EXTERN int32_t sum_int_array(int32_t *, size_t);

DLL_EXTERN void save_ocaml_value(void *);
DLL_EXTERN void *retrieve_ocaml_value(void);

DLL_EXTERN int sixargs(int, int, int, int, int, int);
DLL_EXTERN int return_10(void);
DLL_EXTERN void return_void(int *);

DLL_EXTERN int callback_returns_char_a(char (*)(void));

DLL_EXTERN uint8_t callback_returns_uint8_t(uint8_t (*f)(void));
DLL_EXTERN uint16_t callback_returns_uint16_t(uint16_t (*f)(void));
DLL_EXTERN uint32_t callback_returns_uint32_t(uint32_t (*f)(void));
DLL_EXTERN uint64_t callback_returns_uint64_t(uint64_t (*f)(void));

DLL_EXTERN int8_t callback_returns_int8_t(int8_t (*f)(void));
DLL_EXTERN int16_t callback_returns_int16_t(int16_t (*f)(void));
DLL_EXTERN int32_t callback_returns_int32_t(int32_t (*f)(void));
DLL_EXTERN int64_t callback_returns_int64_t(int64_t (*f)(void));

DLL_EXTERN float callback_returns_float(float (*f)(void));
DLL_EXTERN double callback_returns_double(double (*f)(void));
DLL_EXTERN bool callback_returns_bool(bool (*f)(void));

DLL_EXTERN char *string_array[2];
DLL_EXTERN int32_t int_array[5];

DLL_EXTERN void check_ones(const int *, size_t);

DLL_EXTERN intnat max_caml_int(void);

DLL_EXTERN int foreign_thread_registration_test(void (*)(uint64_t),unsigned,unsigned);

DLL_EXTERN int call_dynamic_funptr(int (*)(int),int);

DLL_EXTERN void save_dynamic_funptr(int (*)(int));
DLL_EXTERN int call_saved_dynamic_funptr(int);

struct simple_closure { int (*f)(int); int n; };
DLL_EXTERN int call_dynamic_funptr_struct(struct simple_closure);
DLL_EXTERN int call_dynamic_funptr_struct_ptr(struct simple_closure*);

#endif /* TEST_FUNCTIONS_H */
