//
// File: main.cpp
//
// MATLAB Coder version            : 3.2
// C/C++ source code generated on  : 19-Oct-2018 17:06:17
//

//***********************************************************************
// This automatically generated example C main file shows how to call
// entry-point functions that MATLAB Coder generated. You must customize
// this file for your application. Do not modify this file directly.
// Instead, make a copy of this file, modify it, and integrate it into
// your development environment.
//
// This file initializes entry-point function arguments to a default
// size and value before calling the entry-point functions. It does
// not store or use any values returned from the entry-point functions.
// If necessary, it does pre-allocate memory for returned values.
// You can use this file as a starting point for a main function that
// you can deploy in your application.
//
// After you copy the file, and before you deploy it, you must make the
// following changes:
// * For variable-size function arguments, change the example sizes to
// the sizes that your application requires.
// * Change the example values of function arguments to the values that
// your application requires.
// * If the entry-point functions return values, store these values or
// otherwise use them as required by your application.
//
//***********************************************************************
// Include Files
#include<Rcpp.h>

#include "include/rt_nonfinite.h"
#include "include/NewModel1Clean.h"
#include "include/main.h"
#include "include/NewModel1Clean_terminate.h"
#include "include/NewModel1Clean_emxAPI.h"
#include "include/NewModel1Clean_initialize.h"


// Function Declarations
//..// static emxArray_real_T *argInit_1xUnbounded_real_T();
emxArray_real_T *argInit_1xUnbounded_real_T( int dim, int irows, int icols, Rcpp::NumericVector data);
static float argInit_real_T();
Rcpp::NumericVector main_NewModel1Clean(Rcpp::RObject odata );

// Function Definitions

//
// Arguments    : void
// Return Type  : emxArray_real_T *
//
//static emxArray_real_T *argInit_1xUnbounded_real_T()
emxArray_real_T *argInit_1xUnbounded_real_T( int dim, int irows, int icols, Rcpp::NumericVector data)
{
  emxArray_real_T *result;
  int iv0[] = { irows, icols };

  int idx1;

  // Set the size of the array.
  // Change this size to the value that the application requires.
  result = emxCreateND_real_T(2, *(int (*)[2])&iv0[0]);


  // Loop over the array to initialize each element.
  for (idx1 = 0; idx1 < result->allocatedSize; idx1++) {
    // Set the value of the array element.
    // Change this value to the value that the application requires.
    //result->data[result->size[0] * idx1] = argInit_real_T();
    result->data[idx1] = data[idx1];
  }


  return result;
}

//
// Arguments    : void
// Return Type  : float
//
static float argInit_real_T()
{
  return 0.0F;
}

//
// Arguments    :
//    idat : Number of observed data x column
//    odata : data
// Return Type  : void
//
// [[Rcpp::export]]
Rcpp::NumericVector main_NewModel1Clean(Rcpp::RObject odata )
{

  Rcpp::NumericMatrix data = Rcpp:: as<Rcpp::NumericMatrix>(odata);

  emxArray_real_T *x1;
  int idat = 353;
  int icount= (data.length() / idat);
  double b_y1[icount];

  x1 = argInit_1xUnbounded_real_T(2, idat, icount, data);

  // Call the entry-point 'NewModel1Clean'.
  NewModel1Clean(x1, b_y1);

  emxDestroyArray_real_T(x1);

  Rcpp::NumericVector age( sizeof(b_y1)/sizeof(double) );

  for (int idq = 0; idq < sizeof(b_y1)/sizeof(double); idq++) {
    age[idq] = b_y1[idq];
  }

  return Rcpp::wrap(age);

}

//
// Arguments    : int argc
//                const char * const argv[]
// Return Type  : int
//
/*
int main(int, const char * const [])
{
  // Initialize the application.
  // You do not need to do this more than one time.
  NewModel1Clean_initialize();

  // Invoke the entry-point functions.
  // You can call entry-point functions multiple times.
  //..// main_NewModel1Clean();

  // Terminate the application.
  // You do not need to do this more than one time.
  NewModel1Clean_terminate();
  return 0;
}
*/
//
// File trailer for main.cpp
//
// [EOF]
//

