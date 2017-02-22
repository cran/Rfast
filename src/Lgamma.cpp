//Author: Manos Papadakis


#include <RcppArmadillo.h>
#include <Rinternals.h>
#include <R.h>
#include "mn.h"

using namespace Rcpp;
using namespace std;

//[[Rcpp::export]]
SEXP Lgamma(SEXP x){
  int n=LENGTH(x);
  SEXP f=PROTECT(Rf_duplicate(x));
  switch(TYPEOF(x)){
    case REALSXP:{
      double *start_f=REAL(f),*start_x=REAL(x),*end_x=start_x+n;
      for(;start_x!=end_x;++start_x,++start_f)
        *start_f=lgamma(*start_x);
      break;
    }
    default:{
      int *start_f=INTEGER(f),*start_x=INTEGER(x),*end_x=start_x+n;
      for(;start_x!=end_x;++start_x,++start_f)
        *start_f=lgamma(*start_x);
      break;
    }
  }
  UNPROTECT(1);
  return f;
}

RcppExport SEXP Rfast_Lgamma(SEXP x){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    __result = Lgamma(x);
    return __result;
END_RCPP
}
