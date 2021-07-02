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
#include <Rcpp.h>
// Function Definitions

//
// Arguments    : const double a[1059]
//                const double b[353]
//                double c[1059]
// Return Type  : void
//
void b_bsxfun( double *a,  double *b, double *c, int cpgs, int samples)
{
  int ak = 0;
  int ck;
  int k;
  
  for (ck = 0; ck <= (cpgs*(samples-1))+1; ck += cpgs) {
    for (k = 0; k < cpgs; k++) {
      c[ck + k] = a[ak + k] * b[k];
    }
    
    ak += cpgs;
  }
}

//
// Arguments    : const double a[1059]
//                const double b[353]
//                double c[1059]
// Return Type  : void
//
void bsxfun( double *a,  double *b, double *c, int cpgs, int samples)
{
  int ak = 0;
  int ck;
  int k;
  
  for (ck = 0; ck <= (cpgs*(samples-1))+1; ck += cpgs) {
    for (k = 0; k < cpgs; k++) {
      c[ck + k] = a[ak + k] - b[k];
    }
    
    ak += cpgs;
  }
}

//
// Arguments    : const double a[1059]
//                double c[1059]
// Return Type  : void
//
void c_bsxfun( double *a, double *c, int cpgs, int samples)
{
  int ak = 0;
  int ck;
  int k;
  
  for (ck = 0; ck <= (cpgs*(samples-1))+1; ck += cpgs) {
    for (k = 0; k < cpgs; k++) {
      c[ck + k] = a[ak + k] - 1.0;
    }
    
    ak += cpgs;
  }
}

//
// File trailer for bsxfun.cpp
//
// [EOF]
//
