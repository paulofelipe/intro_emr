
// includes from the plugin

#include <Rcpp.h>


#ifndef BEGIN_RCPP
#define BEGIN_RCPP
#endif

#ifndef END_RCPP
#define END_RCPP
#endif

using namespace Rcpp;

// user includes
#include <math.h>

// declarations
extern "C" {
SEXP rcppTensor_231715453a83cc2c17001e95491bef2d( SEXP A_, SEXP j_, SEXP i_) ;
}

// definition

SEXP rcppTensor_231715453a83cc2c17001e95491bef2d( SEXP A_, SEXP j_, SEXP i_ ){
BEGIN_RCPP
Rcpp::NumericVector A(A_);
int j_n = Rcpp::as<int> (j_);
int i_n = Rcpp::as<int> (i_);
 int j_i;
int i_i;
Rcpp::NumericVector R(j_n*i_n);
 for (j_i=0; j_i<j_n; j_i++)
for (i_i=0; i_i<i_n; i_i++)
 {
A[i_i + i_n*( j_i )] = A[i_i + i_n*( j_i )] + \
 A[i_i+i_n*(j_i)]; 
 }
 return(wrap(R));

END_RCPP
}



