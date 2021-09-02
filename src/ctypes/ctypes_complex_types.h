#ifndef CTYPES_COMPLEX_TYPES_H
#define CTYPES_COMPLEX_TYPES_H

#ifdef _MSC_VER
# include <complex.h> /* see https://docs.microsoft.com/en-us/cpp/c-runtime-library/complex-math-support?view=msvc-160 */
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

#endif /* CTYPES_COMPLEX_TYPES_H */
