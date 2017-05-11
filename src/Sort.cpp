//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include <R.h>
#include <Rinternals.h>
#include <algorithm>
#include "mn.h"

using namespace Rcpp;

//[[Rcpp::export]]
SEXP Sort(SEXP x,const bool descend){
  SEXP f=PROTECT(Rf_duplicate(x));
  int len=LENGTH(x);
  switch(TYPEOF(x)){
    case INTSXP:{
      int *F=INTEGER(f);
      if(descend)
        sort(F,F+len,descending_int);
      else 
        sort(F,F+len);
      break;
    }
    default:{
      double *F=REAL(f);
      if(descend)
        sort(F,F+len,descending_double);
      else 
        sort(F,F+len);
      break;
    }
  }
  UNPROTECT(1);
  return f;
}

RcppExport SEXP Rfast_Sort(SEXP x,SEXP descendSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< const bool >::type descend(descendSEXP);
    __result = Sort(x,descend);
    return __result;
END_RCPP
}
