#ifndef CTYPES_COMPLEX_TYPES_H
#define CTYPES_COMPLEX_TYPES_H

#ifdef _MSC_VER
# include <complex.h> /* see https://docs.microsoft.com/en-us/cpp/c-runtime-library/complex-math-support?view=msvc-160 */
#endif

#ifdef _MSC_VER
typedef _Lcomplex            longdoublecomplex_t;
typedef _Dcomplex            doublecomplex_t;
typedef _Fcomplex            floatcomplex_t;
#else
typedef long double _Complex longdoublecomplex_t;
typedef double _Complex      doublecomplex_t;
typedef float _Complex       floatcomplex_t;
#endif

#endif /* CTYPES_COMPLEX_TYPES_H */
