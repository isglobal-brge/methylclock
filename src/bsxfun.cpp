//
// File: bsxfun.cpp
//
// MATLAB Coder version            : 3.2
// C/C++ source code generated on  : 23-Nov-2018 14:11:15
//

// Include Files
#include "include/rt_nonfinite.h"
#include "include/NewModel1Clean.h"
#include "include/bsxfun.h"

// Function Definitions

//
// Arguments    : const double a[1059]
//                const double b[353]
//                double c[1059]
// Return Type  : void
//
void b_bsxfun(const double a[], const double b[353], double c[], int sizealloc)
{
  int ak;
  int ck;
  int k;
  ak = 0;
  for (ck = 0; ck <= (sizealloc-352); ck += 353) {
    for (k = 0; k < 353; k++) {
      c[ck + k] = a[ak + k] * b[k];
    }

    ak += 353;
  }
}

//
// Arguments    : const double a[1059]
//                const double b[353]
//                double c[1059]
// Return Type  : void
//
void bsxfun(const double a[], const double b[353], double c[], int sizealloc)
{
  int ak;
  int ck;
  int k;
  ak = 0;
  for (ck = 0; ck <= (sizealloc-352); ck += 353) {
    for (k = 0; k < 353; k++) {
      c[ck + k] = a[ak + k] - b[k];
    }

    ak += 353;
  }
}

//
// Arguments    : const double a[1059]
//                double c[1059]
// Return Type  : void
//
void c_bsxfun(const double a[], double c[], int sizealloc)
{
  int ak;
  int ck;
  int k;
  ak = 0;
  for (ck = 0; ck <= (sizealloc-352); ck += 353) {
    for (k = 0; k < 353; k++) {
      c[ck + k] = a[ak + k] + -1.0;
    }

    ak += 353;
  }
}

//
// File trailer for bsxfun.cpp
//
// [EOF]
//
