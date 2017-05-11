//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include <R.h>
#include <Rinternals.h>
#include "mn.h"

using namespace Rcpp;

//[[Rcpp::export]]
SEXP stable_sort(SEXP x,const bool descend){
  SEXP f=PROTECT(Rf_duplicate(x));
  int len=LENGTH(x);
  switch(TYPEOF(x)){
    case INTSXP:{
      int *F=INTEGER(f);
      if(descend)
        stable_sort(F,F+len,descending_int);
      else 
        stable_sort(F,F+len);
      break;
    }
    default:{
      double *F=REAL(f);
      if(descend)
        stable_sort(F,F+len,descending_double);
      else 
        stable_sort(F,F+len);
      break;
    }
  }
  UNPROTECT(1);
  return f;
}

RcppExport SEXP Rfast_stable_sort(SEXP x,SEXP descendSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< const bool >::type descend(descendSEXP);
    __result = stable_sort(x,descend);
    return __result;
END_RCPP
}
