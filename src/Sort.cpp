//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include <R.h>
#include <Rinternals.h>
#include <algorithm>
#include <vector>
#include <string>
#include "mn.h"

using namespace Rcpp;

//[[Rcpp::export]]
SEXP Sort(SEXP x,const bool descend){
  SEXP f=PROTECT(Rf_duplicate(x));
  int len=LENGTH(x);
  switch(TYPEOF(x)){
    case INTSXP:{
      int *F=INTEGER(f);
      descend ? sort(F,F+len,descending_int) : sort(F,F+len);
      break;
    }
    default:{
      double *F=REAL(f);
      descend ? sort(F,F+len,descending_double) : sort(F,F+len);
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

//[[Rcpp::export]]
vector<string> sort_string(CharacterVector x,const bool descend){
  vector<string> f(x.begin(),x.end());
  if(descend)
  	sort(f.begin(),f.end(),descending_string);
  else
  	sort(f.begin(),f.end());
  return f;
}

RcppExport SEXP Rfast_sort_string(SEXP xSEXP,SEXP descendSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< CharacterVector >::type x(xSEXP);
    traits::input_parameter< const bool >::type descend(descendSEXP);
    __result = wrap(sort_string(x,descend));
    return __result;
END_RCPP
}
