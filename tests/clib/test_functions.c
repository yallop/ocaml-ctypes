#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdint.h>

typedef int intfun(int, int);

static int add(int x, int y) { return x + y; }
static int times(int x, int y) { return x * y; }

int higher_order_1(intfun *callback, int x, int y)
{
  return callback(x, y) == x + y;
}

typedef int acceptor(intfun *, int, int);

int higher_order_3(acceptor *callback, intfun *fn, int x, int y)
{
  return callback(fn, x, y);
}

typedef int vintfun(int);

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
  default: return NULL; /* TODO: can we handle null function pointers? */
  }
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
  fprintf(stderr, "[before] x = %d, y = %d, &x = %p, &y = %p, p = %p, &p = %p\n",
          x, y, &x, &y, p, &p);

  fprintf(stderr, "calling f(%p, %p)\n", &p, &y);
          
  
  int rv = f(&p, &y);
  fprintf(stderr, "[after] x = %d, y = %d, p = %p, *p = %d\n",
          x, y, p, *p);
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

struct simple {
  int i;
  double f;
  struct simple *self;
};

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

union padded {
  int64_t i;
  char    a[sizeof(int64_t) + 1];
};

int64_t sum_union_components(union padded *padded, size_t len)
{
  size_t i;
  int64_t acc = 0;
  for (i = 0; i < len; i++) {
    acc += padded[i].i;
  }
  return acc;
}

