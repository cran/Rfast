//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include "mn.h"

using namespace Rcpp;

//[[Rcpp::export]]
SEXP col_true(SEXP x){
  const int p=Rf_nrows(x);
  SEXP f=Rf_allocVector(INTSXP,p);
  int *ff=INTEGER(f),*xx=LOGICAL(x),*endx=xx+LENGTH(x);
  for(;xx!=endx;xx+=p,++ff)
    *ff=True(xx,xx+p);
  return f;
}

RcppExport SEXP Rfast_col_true(SEXP x){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    __result = col_true(x);
    return __result;
END_RCPP
}
