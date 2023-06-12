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
static float argInit_real_T();
Rcpp::NumericVector main_NewModel1Clean(Rcpp::RObject odata );

// Function Definitions


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
  
  Rcpp::NumericMatrix data = Rcpp::as<Rcpp::NumericMatrix>(odata);
  
  int icpgs = data.nrow(); // Common CpGs
  int isamples= data.ncol(); // Samples
  double b_y1[isamples];
  
  
  // Call the entry-point 'NewModel1Clean'.
  NewModel1Clean(data, b_y1, icpgs, isamples);
  
  return Rcpp::as<Rcpp::NumericVector>(Rcpp::wrap(std::vector<double>( b_y1, b_y1+sizeof(b_y1)/sizeof(*b_y1))));
  
  
}
