//
// File: repmat.cpp
//
// MATLAB Coder version            : 3.2
// C/C++ source code generated on  : 23-Nov-2018 14:11:15
//

// Include Files
#include "include/rt_nonfinite.h"
#include "include/NewModel1Clean.h"
#include "include/repmat.h"

// Function Definitions

//
// Arguments    : double b[15]
// Return Type  : void
//
void repmat(double b[], int sizeobs)
{
  int jtilecol;
  int ibtile;
  int k;
  static const double a[5] = { 0.015748339881932315, -0.0071748790184208641,
    0.029749113663867798, 0.0033606020505486025, 0.0029044623730544641 };

  for (jtilecol = 0; jtilecol < sizeobs; jtilecol++) {
    ibtile = jtilecol * 5;
    for (k = 0; k < 5; k++) {
      b[ibtile + k] = a[k];
    }
  }
}

//
// File trailer for repmat.cpp
//
// [EOF]
//
