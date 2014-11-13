#include "bench_micro_stubs.h"

int f_i0() { return 0; }
int f_i1(int i0) { return i0; }
int f_i2(int i0, int i1) { return i1; }
int f_i3(int i0, int i1, int i2) { return i2; }
int f_i4(int i0, int i1, int i2, int i3) { return i3; }
int f_i5(int i0, int i1, int i2, int i3, int i4) { return i4; }
int f_i6(int i0, int i1, int i2, int i3, int i4, int i5) { return i5; }
int f_i7(int i0, int i1, int i2, int i3, int i4, int i5, int i6) { return i6; }
int f_i8(int i0, int i1, int i2, int i3, int i4, int i5, int i6, int i7) {
  return i7;
}
int f_i9(int i0, int i1, int i2, int i3, int i4, int i5, int i6, int i7, int i8)
{
  return i8;
}
