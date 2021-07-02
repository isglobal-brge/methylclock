//
// File: main.cpp
//
// MATLAB Coder version            : 3.2
// C/C++ source code generated on  : 19-Oct-2018 17:06:17
// 
// 
//    NOTE : Adapted to R using C++ with Rcpp
//




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
  int icpgs = data.nrow(); // Common CpGs
  int isamples= data.ncol(); // Samples
  double b_y1[isamples];
  
  // Call the entry-point 'NewModel1Clean'.
  NewModel1Clean(data, b_y1, icpgs, isamples);
  
  Rcpp::NumericVector age( isamples );
  
  for (int idq = 0; idq < isamples; idq++) {
    age[idq] = b_y1[idq];
  }
  
  return Rcpp::wrap(age);
  
}
